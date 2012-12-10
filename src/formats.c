#include <libelf.h>
#include <stdlib.h>

typedef unsigned char byte;

#define SETSTRUCT(v,t,vals...) { t tmp = vals; v = tmp; }
#define DEF_FORMAT(fmt,base,htype,init...)              \
    int const fmt##_HSIZE = sizeof(htype);              \
    int const fmt##_ENTRY = sizeof(htype) + (base);     \
    int fmt##_entry() { return fmt##_ENTRY; }           \
    int fmt##_headerSize() { return fmt##_HSIZE; }      \
    byte* fmt##_headerCons(int dataSize) {              \
        htype* ret = malloc(sizeof(htype));             \
        SETSTRUCT(*ret,htype,init);                     \
        return (byte*)ret;                              \
    }

typedef struct {
    Elf64_Ehdr eh;
    Elf64_Phdr pht[1];
} elf64_Header;
DEF_FORMAT(elf64,1<<21,elf64_Header,{
    .eh = {
        .e_ident = { 
            ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
            ELFCLASS64, ELFDATA2LSB,
            EV_CURRENT,
            ELFOSABI_NONE, 0,
            0
        },
        .e_type      = ET_EXEC,
        .e_machine   = EM_X86_64,
        .e_version   = EV_CURRENT,
        .e_entry     = elf64_ENTRY,
        .e_phoff     = sizeof(Elf64_Ehdr),
        .e_shoff     = 0,
        .e_flags     = 0,
        .e_ehsize    = sizeof(Elf64_Ehdr),
        .e_phentsize = sizeof(Elf64_Phdr),
        .e_phnum     = 1,
        .e_shentsize = 0,
        .e_shnum     = 0,
        .e_shstrndx  = SHN_UNDEF
    },
    .pht = {{ 
        .p_type   = PT_LOAD,
        .p_offset = elf64_HSIZE,
        .p_vaddr  = elf64_ENTRY,
        .p_paddr  = 0,
        .p_filesz = dataSize,
        .p_memsz  = dataSize,
        .p_flags  = PF_R | PF_W | PF_X,
        .p_align  = 1<<21
    }}})

typedef struct {
    Elf32_Ehdr eh;
    Elf32_Phdr pht[1];
} elf32_Header;
DEF_FORMAT(elf32,1<<21,elf32_Header,{
    .eh = {
        .e_ident = { 
            ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
            ELFCLASS32, ELFDATA2LSB,
            EV_CURRENT,
            ELFOSABI_NONE, 0,
            0
        },
        .e_type      = ET_EXEC,
        .e_machine   = EM_386,
        .e_version   = EV_CURRENT,
        .e_entry     = elf32_ENTRY,
        .e_phoff     = sizeof(Elf32_Ehdr),
        .e_shoff     = 0,
        .e_flags     = 0,
        .e_ehsize    = sizeof(Elf32_Ehdr),
        .e_phentsize = sizeof(Elf32_Phdr),
        .e_phnum     = 1,
        .e_shentsize = 0,
        .e_shnum     = 0,
        .e_shstrndx  = SHN_UNDEF
    },
    .pht = {{ 
        .p_type   = PT_LOAD,
        .p_offset = elf32_HSIZE,
        .p_vaddr  = elf32_ENTRY,
        .p_paddr  = 0,
        .p_filesz = dataSize,
        .p_memsz  = dataSize,
        .p_flags  = PF_R | PF_W | PF_X,
        .p_align  = 1<<21
    }}})

typedef struct {
  unsigned short signature; /* == 0x5a4D */
  unsigned short bytes_in_last_block;
  unsigned short blocks_in_file;
  unsigned short num_relocs;
  unsigned short header_paragraphs;
  unsigned short min_extra_paragraphs;
  unsigned short max_extra_paragraphs;
  unsigned short ss;
  unsigned short sp;
  unsigned short checksum;
  unsigned short ip;
  unsigned short cs;
  unsigned short reloc_table_offset;
  unsigned short overlay_number;
} exe_Header;
DEF_FORMAT(exe,0x1000,exe_Header,{
    .signature = 0x5a4d,
    .bytes_in_last_block = (dataSize+exe_HSIZE)%512,
    .blocks_in_file = (dataSize+exe_HSIZE)/512,
    .num_relocs = 0,
    .header_paragraphs = 1,
    .min_extra_paragraphs = 0,
    .max_extra_paragraphs = 0,
    .ss = 0,
    .sp = 0x1000,
    .checksum = 0,
    .ip = exe_ENTRY,
    .cs = 0,
    .reloc_table_offset = 0,
    .overlay_number = 0
})
