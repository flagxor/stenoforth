#include "stenoforth.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
# include "windows.h"
#else
# include <elf.h>
#endif

#define HEAP_SIZE (1024 * 1024)
#define DSTACK_SIZE 4096
#define RSTACK_SIZE 4096

extern unsigned char boot_dat[];

cell_t *vm_load(const char *filename) {
  unsigned char *boot = 0;

  FILE *source = fopen(filename, "rb");
  if (!source) {
    fprintf(stderr, "Cannot open %s\n", filename);
    exit(1);
  }
  fseek(source, 0, SEEK_END);
  int size = ftell(source);
#ifdef _WIN32
  // Figure out where the exe ends.
  fseek(source, 30 * 2, SEEK_SET);  // offset to new header
  uint32_t newheader = 0;
  fread(&newheader, sizeof(newheader), 1, source);
  // Get number of sections
  fseek(source, newheader + 4 + 2, SEEK_SET);
  uint16_t sections = 0;
  fread(&sections, sizeof(sections), 1, source);
  // Get size of option header
  fseek(source, newheader + 4 + 4 * 4, SEEK_SET);
  uint16_t optional_header_size = 0;
  fread(&optional_header_size, sizeof(optional_header_size), 1, source);
  // Gather total size.
  uint32_t base = newheader + 6 * 4 + optional_header_size;
  uint32_t start = 0;
  for (int i = 0; i < sections; ++i) {
    fseek(source, base + i * 10 * 4 + 4 * 4, SEEK_SET);   // SizeOfRawData, PointerToRawData
    uint32_t raw_data = 0;
    fread(&raw_data, sizeof(raw_data), 1, source);
    uint32_t pointer_to_raw_data = 0;
    fread(&pointer_to_raw_data, sizeof(pointer_to_raw_data), 1, source);
    uint32_t end = pointer_to_raw_data + raw_data;
    if (end > start) {
      start = end;
    }
  }
#else
  // Figure out where the elf binary ends.
  fseek(source, 4, SEEK_SET);
  int class = fgetc(source);
  fseek(source, 0, SEEK_SET);
  int start = 0;
  if (class == ELFCLASS32) {
    Elf32_Ehdr header;
    if (fread(&header, sizeof(header), 1, source) != 1) {
      fprintf(stderr, "Failed to read executable!\n");
      exit(1);
    }
    start = header.e_shoff + (header.e_shentsize * header.e_shnum);
  } else if (class == ELFCLASS64) {
    Elf64_Ehdr header;
    if (fread(&header, sizeof(header), 1, source) != 1) {
      fprintf(stderr, "Failed to read executable!\n");
      exit(1);
    }
    start = header.e_shoff + (header.e_shentsize * header.e_shnum);
  } else {
    fprintf(stderr, "Bad elf class\n");
    exit(1);
  }
#endif
  size_t length;
  size_t heap_size = HEAP_SIZE + DSTACK_SIZE + RSTACK_SIZE;
  if (0 && start != size) {
    length = size - start;
  } else {
    length = ((uint32_t*) boot_dat)[0] * 4;
  }
  heap_size += length;
  boot = malloc(heap_size);
  if (!boot) {
    fprintf(stderr, "Cannot allocate heap\n");
    exit(1);
  }
  if (0 && start != size) {
    fseek(source, start, SEEK_SET);
    if (fread(boot, size - start, 1, source) != 1) {
      fprintf(stderr, "Failed to read executable extension\n");
      exit(1);
    }
  } else {
    memcpy(boot, boot_dat, length);
  }
  fclose(source);

  // Layout stack and startup params
  cell_t *dstack = (cell_t *) &boot[HEAP_SIZE];
  cell_t *rstack = (cell_t *) &boot[HEAP_SIZE + DSTACK_SIZE];
  cell_t *rp = rstack;
  dstack[1] = (cell_t) &boot[4 * ((uint32_t *) boot)[0]];
  *++rp = (cell_t) &dstack[1];
  *++rp = (cell_t) &boot[4];

  return rp;
}

