global inverse_permutation

inverse_permutation:


    cmp    rdi,0
    jle bad

    mov    rax,0x80000000
    cmp    rdi,rax
    jae bad


    xor    rdx,rdx ; i = 0

sprawdz_petla:
    movsxd rax, DWORD [rsi+rdx*4] ; rax = p[i]
    cmp   eax, 0              ; eax > 0
    js    bad
    cmp    rax,rdi             ; eax < n
    jae    bad

    add    rdx,0x1
    cmp    rdx,rdi
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
    mov DWORD [r9+4*rdx], ecx
    mov ecx, edx
    mov edx, edi

    mov eax, [r9+4*rdx]
    cmp eax, 0
    jl petla_w_petli

    mov edx, ecx

petla_zakonczeni:
    add    rdx,0x1                      ; ++i;

    cmp rdx, r8
    jb odwroc_petla
    jmp end



bad:
    xor eax, eax
    ret

end:
    mov eax, 0x1
    ret