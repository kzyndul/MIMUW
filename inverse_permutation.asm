global inverse_permutation
inverse_permutation:
        test    edi, edi                ; if (n <= 0) return false
        js      bad

        mov     r8, rdi
        mov     r9, rsi
        mov     rdi, 0x80000000
        mov     rsi, 0x7fffffff
        cmp     r8, rdi                 ; if (n > INT_MAX) return false
        jg      bad

        xor     rdx, rdx                ; i = 0
sprawdz_petla:
        mov     eax, DWORD [r9+rdx*4]   ; rax = p[i]
        test    eax, eax                ; if (p[i] < 0) return false
        js      bad
        cmp     eax, r8d                ; if (p[i] > n) return false
        jae     bad
        inc     edx                     ; ++i
        cmp     edx, r8d                ; while (i < n)
        jne     sprawdz_petla

        xor     edx,edx                 ; i = 0
pierwsza_petla:
        mov     eax, DWORD [r9+rdx*4]   ; eax (dokad) = p[i]
        and     eax, esi                ; eax & 11111111
        lea     rcx, [r9+rax*4]         ; rcx = p[dokad]
        mov     eax, DWORD [rcx]        ; eax = p[dokad]
        test    eax, eax                ; if p[dokad] < 0
        js      popraw                  ; petla do poprawy a nie od razu bad
        or      eax, edi                ; eax = eax | 1000000000000
        mov     DWORD [rcx], eax        ; p[dokad] = p[dokad] | 1000000000
        inc     edx                     ; ++i;
        cmp     edx, r8d                ; i < n
        jb      pierwsza_petla

odwroc:
        xor     edx, edx                ; i = 0
odwroc_petla:
        mov     eax, [r9+4*rdx]
        cmp     eax, 0                  ; if (p[i] < 0)
        jge     petla_zakonczeni
        mov     rcx, rdx                ; poprzedni = i
        mov     eax, DWORD [r9+4*rdx]   ; eax = p[i]
        and     eax, esi                ; eax = eax & 1111
        mov     edx, eax                ; i = p[i]

petla_w_petli:
        mov     edi, [r9+4*rdx]         ; temp = p[i]
        and     edi, esi                ; temp = temp & 111111
        mov     DWORD [r9+4*rdx], ecx   ; p[i] = poprzedni
        mov     ecx, edx                ; poprzedni = i
        mov     edx, edi                ; i  = temp
        mov     eax, [r9+4*rdx]         ; eax = p[i]
        cmp     eax, 0                  ; while (p[i] < 0)
        jl      petla_w_petli
        mov     edx, ecx                ; i = poprzedni

petla_zakonczeni:
        inc     edx                     ; ++i;
        cmp     edx, r8d                ; while (i < n)
        jb      odwroc_petla

end:
        mov     eax, 0x1
        ret

popraw:
        dec     edx

popraw_petla:
        mov     eax, DWORD  [r9+rdx*4]  ; eax = dokad (p[i])
        and     eax, esi                ; dokad = dokad & 1111111
        and     DWORD  [r9+rax*4], esi  ; p[dokad] = p[dokad] & 111111
        dec     edx                     ; --i
        test    edx, edx                ; if (i >= 0)
        jns     popraw_petla

bad:
        xor     eax, eax
        ret