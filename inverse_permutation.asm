global inverse_permutation

inverse_permutation:


    cmp    edi,0        ; if (n <= 0) return false
    jle bad

    mov    rax,0x80000001   ; if (n > INT_MAX) return false
    cmp    rdi,rax
    jge bad


    xor    edx,edx ; i = 0

sprawdz_petla:
    movsxd rax, DWORD [rsi+rdx*4] ; rax = p[i]
    cmp   eax, 0              ; if (p[i] < 0) return false
    js    bad
    cmp    rax,rdi             ; if (p[i] > n) return false
    jae    bad

    add    rdx,0x1              ; ++i
    cmp    rdx,rdi              ; while (i < n)
    jne sprawdz_petla


    xor    edx,edx  ; i = 0
pierwsza_petla:
    mov    eax, DWORD [rsi+rdx*4] ; eax (dokad) = p[i]
    and    eax,0x7fffffff          ; eax & 11111111
    lea    rcx,[rsi+rax*4]         ; rcx = p[dokad]
    mov    eax, DWORD [rcx]         ; eax = p[dokad]
    cmp   eax,0                     ; if p[dokad] < 0 
    js    popraw                       ; petla do poprawy a nie od razu bad

    or     eax,0x80000000              ; eax = eax | 1000000000000
    add    rdx,0x1                      ; ++i;
    mov    DWORD [rcx],eax              ; p[dokad] = p[dokad] | 1000000000
    cmp    rdx, rdi                    ;  i < n
    jb pierwsza_petla
    jmp odwroc



popraw:
    sub    rdx,0x1
    js     bad      ; imo nie potrzeben

popraw_petla:
    mov    eax,DWORD  [rsi+rdx*4] ; eax = dokad (p[i])
    sub    rdx,0x1                ; --i
    and    eax,0x7fffffff         ; dokad = dokad & 1111111
    and    DWORD  [rsi+rax*4],0x7fffffff ; p[dokad] = p[dokad] & 111111
    test   edx,edx                      ; if (i >= 0)
    jns    popraw_petla
    jmp bad


odwroc:


    xor    edx,edx  ; i = 0
    mov r8, rdi
    mov r9, rsi
odwroc_petla:

        ; if (p[i] < 0) 
    mov eax, [r9+4*rdx]
    cmp eax, 0
    jge petla_zakonczeni


    mov rcx, rdx    ; poprzedni = i
    mov eax, DWORD [r9+4*rdx] ; eax = p[i]
    and eax, 0x7fffffff ;eax = eax & 1111
    mov edx, eax ; i = p[i]


petla_w_petli:
    mov edi, [r9+4*rdx] ; temp = p[i]
    and edi, 0x7fffffff ; temp = temp & 111111
    mov DWORD [r9+4*rdx], ecx ; p[i] = poprzedni
    mov ecx, edx ; poprzedni = i
    mov edx, edi ; i  = temp

    mov eax, [r9+4*rdx] ; eax = p[i]
    cmp eax, 0 ; while (p[i] < 0)
    jl petla_w_petli

    mov edx, ecx ; i = poprzedni

petla_zakonczeni:
    add    rdx,0x1                      ; ++i;

    cmp rdx, r8                 ; while (i < n)
    jb odwroc_petla

end:
    mov eax, 0x1
    ret

bad:
    xor eax, eax
    ret