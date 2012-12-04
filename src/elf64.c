#include <libelf.h>
#include <stdlib.h>

#define SETSTRUCT(v,t,vals...) { t tmp = vals; v = tmp; }

typedef unsigned char byte;

typedef struct {
    Elf64_Ehdr eh;
    Elf64_Phdr pht[1];
} Elf64H;
#define H_SIZE (sizeof(Elf64H))
#define ENTRY (H_SIZE+(1<<21))
int elf64_entry() { return ENTRY; }
int elf64_headerSize() { return H_SIZE; }
byte* elf64_headerCons(int dataSize) {
    Elf64H* ret = malloc(sizeof(Elf64H));

    SETSTRUCT(ret->eh,Elf64_Ehdr,{
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
       .e_entry     = ENTRY,
       .e_phoff     = sizeof(Elf64_Ehdr),
       .e_shoff     = 0,
       .e_flags     = 0,
       .e_ehsize    = sizeof(Elf64_Ehdr),
       .e_phentsize = sizeof(Elf64_Phdr),
       .e_phnum     = 1,
       .e_shentsize = 0,
       .e_shnum     = 0,
       .e_shstrndx  = SHN_UNDEF
    });
    SETSTRUCT(ret->pht[0],Elf64_Phdr,{ 
       .p_type   = PT_LOAD,
       .p_offset = H_SIZE,
       .p_vaddr  = ENTRY,
       .p_paddr  = 0,
       .p_filesz = dataSize,
       .p_memsz  = dataSize,
       .p_flags  = PF_R | PF_W | PF_X,
       .p_align  = 1<<21
    });
    
    return (byte*)ret;
}

/* 
Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

