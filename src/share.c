/*
 * kit : Useful R Functions Implemented in C
 * Copyright (C) 2020-2021  Morgan Jacob
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "kit.h"

/*
 *  Structure to hold Length and Address 
 *  of data to be shared in memory segment
 */

struct OBJECT {
#ifdef WIN32
  HANDLE hMapFile;
  HANDLE hMapLength;
  LPCTSTR lpMapAddress;
  LPCTSTR lpMapLength;
#else
  int fd_addr;
  int fd_length;
  size_t STORAGE_SIZE;
  void *addr;
  void *length;
  const char *STORAGE_ID;
  const char *LENGTH_ID;
#endif
};

/*
 *  Utility Function
 */

/*int get_pid () {
#ifdef WIN32
  return GetCurrentProcessId();
#else
  return getpid();
#endif
}*/
/*int get_tid () {
#ifdef WIN32
  return GetCurrentThreadId();
#else
  return gettid();
#endif
}*/

/*
 *  Function to finalize memory map pointer
 */

static bool verbose_finalizer = false;

static void map_finalizer (SEXP ext) {
  if (verbose_finalizer) Rprintf("* Finalize...\n");
  if (NULL == R_ExternalPtrAddr(ext)) {
    return;
  }
  if (verbose_finalizer) Rprintf("* Clear external pointer...\n");
  struct OBJECT *ptr = (struct OBJECT*) R_ExternalPtrAddr(ext);
#ifdef WIN32  
  UnmapViewOfFile(ptr->lpMapAddress);
  CloseHandle(ptr->hMapFile);
  UnmapViewOfFile(ptr->lpMapLength);
  CloseHandle(ptr->hMapLength);
#else
  munmap(ptr->addr, ptr->STORAGE_SIZE);
  shm_unlink(ptr->STORAGE_ID);
  munmap(ptr->length, 256);
  shm_unlink(ptr->LENGTH_ID);
#endif
  Free(ptr);
  R_ClearExternalPtr(ext);
  if (verbose_finalizer) Rprintf("* Clear external pointer...OK\n");
}

/*
 *  Function to create data
 */

SEXP createMappingObjectR (SEXP MapName, SEXP MapLength, SEXP DataObject, SEXP verboseArg) {
  if (TYPEOF(MapName) != STRSXP || LENGTH(MapName) != 1) {
    error("Argument 'MapName' must be of type character and length 1.");
  }
  if (TYPEOF(verboseArg) != LGLSXP || LENGTH(verboseArg) != 1 || LOGICAL(verboseArg)[0] == NA_LOGICAL) {
    error("Argument 'verbose' must be TRUE or FALSE.");
  }
  const bool verbose = asLogical(verboseArg);
  verbose_finalizer = verbose;
  const size_t len = LENGTH(DataObject);
  const size_t BUF_SIZE = len*sizeof(Rbyte);
  if (verbose) Rprintf("* Data object size: %zu\n",len*sizeof(Rbyte));
  if (verbose) Rprintf("* Start mapping object...OK\n");
  struct OBJECT *foo = Calloc(1, struct OBJECT);
#ifdef WIN32 
  LPSTR pMN = (LPSTR) CHAR(STRING_PTR(MapName)[0]);
  LPSTR pML = (LPSTR) CHAR(STRING_PTR(MapLength)[0]);
  foo->hMapFile = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, BUF_SIZE, pMN);
  foo->hMapLength = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, 256, pML);
  if (foo->hMapFile == INVALID_HANDLE_VALUE || foo->hMapLength == INVALID_HANDLE_VALUE) {
#else
  const char *pMN = CHAR(STRING_PTR(MapName)[0]);
  const char *pML = CHAR(STRING_PTR(MapLength)[0]);
  foo->STORAGE_ID = pMN; 
  foo->LENGTH_ID = pML;
  foo->STORAGE_SIZE = BUF_SIZE;
  foo->fd_addr = shm_open(foo->STORAGE_ID, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  foo->fd_length = shm_open(foo->LENGTH_ID, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (foo->fd_addr == -1 || foo->fd_length == -1) {
#endif
    error("* Creating file mapping...ERROR");
  }
  if (verbose) Rprintf("* Creating file maping...OK\n");
#ifdef WIN32  
#else
  struct stat mapstat;
  if (-1 != fstat(foo->fd_addr, &mapstat) && mapstat.st_size == 0) {
    if(ftruncate(foo->fd_addr, BUF_SIZE) == -1) {
      error("* Extend shared memory object (1)...ERROR");
    }
  }
  if (-1 != fstat(foo->fd_length, &mapstat) && mapstat.st_size == 0) {
    if(ftruncate(foo->fd_length, 256) == -1) {
      error("* Extend shared memory object (2)...ERROR");
    }
  }
  if (verbose) Rprintf("* Extend shared memory object...OK\n");
#endif  
  
#ifdef WIN32
  foo->lpMapAddress = (LPCTSTR) MapViewOfFile (foo->hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, BUF_SIZE);
  foo->lpMapLength = (LPCTSTR) MapViewOfFile (foo->hMapLength, FILE_MAP_ALL_ACCESS, 0, 0, 256);
  if (foo->lpMapAddress == NULL || foo->lpMapLength == NULL) {
    CloseHandle(foo->hMapFile);
    CloseHandle(foo->hMapLength);
#else
    foo->addr = mmap(NULL, BUF_SIZE, PROT_WRITE, MAP_SHARED, foo->fd_addr, 0);
    foo->length = mmap(NULL, 256, PROT_WRITE, MAP_SHARED, foo->fd_length, 0);
    if (foo->addr == MAP_FAILED || foo->length == MAP_FAILED) {
      shm_unlink(foo->STORAGE_ID);
      shm_unlink(foo->LENGTH_ID);
#endif
    error("* Map view file...ERROR");
  }
  if (verbose) Rprintf("* Map view file...OK\n");
#ifdef WIN32
  CopyMemory((LPVOID)foo->lpMapAddress, RAW(DataObject), BUF_SIZE);
  CopyMemory((LPVOID)foo->lpMapLength, &len, sizeof(size_t));
#else
  memcpy(foo->addr, RAW(DataObject), BUF_SIZE);
  memcpy(foo->length, &len, sizeof(size_t));
#endif
  if (verbose) Rprintf("* Copy memory...OK\n");
  SEXP ext = PROTECT(R_MakeExternalPtr(foo, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ext, map_finalizer, TRUE);
  if (verbose) Rprintf("* Register finalizer...OK\n");
  UNPROTECT(1);
  return ext;
}

/*
 *  Function to retrieve data
 */

SEXP getMappingObjectR (SEXP MapName, SEXP MapLength, SEXP verboseArg) {
  if (TYPEOF(MapName) != STRSXP || LENGTH(MapName) != 1) {
    error("Argument 'MapName' must be of type character and length 1.");
  }
  if (TYPEOF(verboseArg) != LGLSXP || LENGTH(verboseArg) != 1 || LOGICAL(verboseArg)[0] == NA_LOGICAL) {
    error("Argument 'verbose' must be TRUE or FALSE.");
  }
  const bool verbose = asLogical(verboseArg);
#ifdef WIN32
  LPSTR pMN = (LPSTR) CHAR(STRING_PTR(MapName)[0]);
  LPSTR pML = (LPSTR) CHAR(STRING_PTR(MapLength)[0]);
  HANDLE hMapFile = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, pMN);
  HANDLE hMapLength = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, pML);
  if (hMapFile == INVALID_HANDLE_VALUE || hMapLength == INVALID_HANDLE_VALUE) {
#else
  const char *pMN = CHAR(STRING_PTR(MapName)[0]);
  const char *pML = CHAR(STRING_PTR(MapLength)[0]);
  int fd_addr = shm_open(pMN, O_RDONLY, S_IRUSR | S_IWUSR);
  int fd_length = shm_open(pML, O_RDONLY, S_IRUSR | S_IWUSR);
  if (fd_addr == -1 || fd_length == -1) {
#endif
    error("* Creating file mapping...ERROR");
  }
  if (verbose) Rprintf("* Creating file maping...OK\n");
#ifdef WIN32
  LPCTSTR lpMapLength = (LPCTSTR) MapViewOfFile (hMapLength, FILE_MAP_ALL_ACCESS, 0, 0, 256);
  if (lpMapLength == NULL) {
    CloseHandle(hMapLength);
#else
  void *length = mmap(NULL, 256, PROT_READ, MAP_SHARED, fd_length, 0);
  if (length == MAP_FAILED) {
    shm_unlink(pML);
#endif
    error("* Map view file (length)...ERROR");
  }
  if (verbose) Rprintf("* Map view file (length)...OK\n");
#ifdef WIN32
  size_t len = *(size_t*)lpMapLength;
  LPCTSTR lpMapAddress = (LPCTSTR) MapViewOfFile (hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, len*sizeof(Rbyte));
  if (lpMapAddress == NULL) {
    CloseHandle(hMapFile);
#else
  size_t len = *(size_t*)length;  
  void *addr = mmap(NULL, len*sizeof(Rbyte), PROT_READ, MAP_SHARED, fd_addr, 0);
  if (addr == MAP_FAILED) {
    shm_unlink(pMN);
#endif
    error("* Map view file (address)...ERROR");
  }
  if (verbose) Rprintf("* Map view file (address)...OK\n");
  SEXP ans = PROTECT(allocVector(RAWSXP, len));
  if (verbose) Rprintf("* Create RAW Vector...OK\n");
#ifdef WIN32
  CopyMemory(RAW(ans), (Rbyte*)lpMapAddress, len*sizeof(Rbyte));
#else
  memcpy(RAW(ans), (Rbyte*)addr, len*sizeof(Rbyte)); // maybe need +1
#endif
  if (verbose) Rprintf("* Copy map memory...OK\n");
  
#ifdef WIN32
  if (!UnmapViewOfFile(lpMapLength)) {
#else
  if (munmap(length, 256) == -1) {
#endif
    error("* Closing mapping file (length)...ERROR");
  }
  if (verbose) Rprintf("* Closing mapping file (length)...OK\n");
#ifdef WIN32
  if (!CloseHandle(hMapLength)) {
#else
  if (shm_unlink(pML) == -1) {
#endif
    error("* Closing mapping handle (length)...ERROR");
  }
  if (verbose) Rprintf("* Closing mapping handle (length)...OK\n");
  
#ifdef WIN32
  if (!UnmapViewOfFile(lpMapAddress)) {
#else
  if (munmap(length, len*sizeof(Rbyte)) == -1) {
#endif
    error("* Closing mapping file (address)...ERROR");
  }
  if (verbose) Rprintf("* Closing mapping file (address)...OK\n");
#ifdef WIN32
  if (!CloseHandle(hMapFile)) {
#else
  if (shm_unlink(pMN) == -1) {
#endif
    error("* Closing mapping handle (address)...ERROR");
  }
  if (verbose) Rprintf("* Closing mapping handle (address)...OK\n");
  UNPROTECT(1);
  return ans;
}

SEXP clearMappingObjectR  (SEXP ext, SEXP verboseArg) {
  if (TYPEOF(verboseArg) != LGLSXP || LENGTH(verboseArg) != 1 || LOGICAL(verboseArg)[0] == NA_LOGICAL) {
    error("Argument 'verbose' must be TRUE or FALSE.");
  }
  const bool verbose = asLogical(verboseArg);
  if (verbose) Rprintf("* Finalize...\n");
  if (NULL == R_ExternalPtrAddr(ext)) {
    return ScalarLogical(FALSE);
  }
  if (verbose) Rprintf("* Clear external pointer...\n");
  struct OBJECT *ptr = (struct OBJECT*) R_ExternalPtrAddr(ext);
#ifdef WIN32  
  UnmapViewOfFile(ptr->lpMapAddress);
  CloseHandle(ptr->hMapFile);
  UnmapViewOfFile(ptr->lpMapLength);
  CloseHandle(ptr->hMapLength);
#else
  munmap(ptr->addr, ptr->STORAGE_SIZE);
  shm_unlink(ptr->STORAGE_ID);
  munmap(ptr->length, 256);
  shm_unlink(ptr->LENGTH_ID);
#endif
  Free(ptr);
  R_ClearExternalPtr(ext);
  if (verbose) Rprintf("* Clear external pointer...OK\n");
  return ScalarLogical(TRUE);
}
