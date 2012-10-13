#include <unistd.h>
#include <libelf.h>
#include "writeElf.h"

#define SETSTRUCT(v,t,vals...) { t tmp = vals; v = tmp; }

typedef unsigned char byte;

void writeElf(int fd,byte* data,int dataSize);

void writeElf(int fd,byte* data,int dataSize) {
   Elf64_Ehdr eh;    Elf64_Phdr pht[1];
   unsigned int hSize = sizeof(eh) + sizeof(pht);

   SETSTRUCT(eh,Elf64_Ehdr,
             {
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
                       .e_entry     = 0,
                       .e_phoff     = sizeof(Elf64_Ehdr),
                       .e_shoff     = 0,
                       .e_flags     = 0,
                       .e_ehsize    = sizeof(Elf64_Ehdr),
                       .e_phentsize = sizeof(Elf64_Phdr),
                       .e_phnum     = 2,
                       .e_shentsize = 0,
                       .e_shnum     = 0,
                       .e_shstrndx  = SHN_UNDEF
                       });
   SETSTRUCT(pht[0],Elf64_Phdr,{ .p_type   = PT_LOAD,
            .p_offset = hSize,
            .p_vaddr  = 0,
            .p_paddr  = 0,
            .p_filesz = dataSize,
            .p_memsz  = dataSize,
            .p_flags  = PF_R | PF_W | PF_X,
            .p_align  = 1
               });
    
   write(fd,&eh,sizeof(eh));
   write(fd,&pht,sizeof(pht));
   write(fd,data,dataSize);
}

/* 
Copyright (c) 2012, Coiffier Marc <marc.coiffier@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DPCodeECT, INDPCodeECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

