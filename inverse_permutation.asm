global inverse_permutation
inverse_permutation:
        test    edi, edi                ; sprawdzam czy n > 0
        js      bad

        mov     r8, 0x80000000          ; przenosze dwie stale do rejestrow
        mov     r9, 0x7fffffff
        cmp     rdi, r8                 ; sprawdzam czy n < INT_MAX + 2 dla n
        jg      bad                     ; wiekszych funkcjia zawsze powinna
                                        ; zwrocic false
        xor     rdx, rdx                ; i = 0
sprawdz_petla:
        movsxd  rax, DWORD [rsi+rdx*4]  ; rax = p[i]
        test    eax, eax                ; sprawdzam czy p[i] >= 0
        js      bad

        cmp     eax, edi                ; sprawdzam czy p[i] < n
        jae     bad

        inc     edx                     ; ++i
        cmp     edx, edi                ; while (i < n)
        jne     sprawdz_petla

        xor     edx,edx                 ; i = 0


; sprawdzam czy tablica p jest poprawna permutacja. Przechodze przez cala
; tablice gdy jestem na pozycji i to zapalam ostatni znak liczbie na pozycji
; p[p[i]] co oznacza ze juz ja spotkalem w permutacji.
pierwsza_petla:
        mov     eax, DWORD [rsi+rdx*4]  ; eax = p[i]
        and     eax, r9d                ; pozbywam sie oznaczenia
        lea     rcx, [rsi+rax*4]        ; rcx = &p[p[i]]
        mov     eax, DWORD [rcx]        ; eax = p[]
        test    eax, eax                ; sprawdzam czy p[i] bylo juz
                                        ; odwiedzone
        js      popraw
        or      eax, r8d                ; zaznaczam jako odwiedzone
        mov     DWORD [rcx], eax        ; zapisuje do tablicy
        inc     edx                     ; ++i
        cmp     edx, edi                ; while (i < n)
        jb      pierwsza_petla

odwroc:
        xor     edx, edx                ; i = 0
; petla odwracajaca permutacje. Jezeli jakas liczba jest ujemna to oznacza,
; ze jeszcze nie zostala odwrucona.
odwroc_petla:
        mov     eax, [rsi+4*rdx]        ; eax = p[i]
        test    eax, eax                ; if (p[i] < 0) nie odwrocony element
        jns     petla_zakonczeni

; odwracam caly cykl w ktorym jest i ty element.
        mov     ecx, edx                ; poprzedni = i
        and     eax, r9d                ; pozbywam sie oznaczenia
        mov     edx, eax                ; i = p[i]

petla_w_petli:
        mov     r8d, [rsi+4*rdx]        ; temp = p[i]
        and     r8d, r9d                ; temp = temp & 111111
        mov     DWORD [rsi+4*rdx], ecx  ; p[i] = poprzedni
        mov     ecx, edx                ; poprzedni = i
        mov     edx, r8d                ; i  = temp
        mov     eax, [rsi+4*rdx]        ; eax = p[i]
        test    eax, eax                ; while (p[i] < 0)
        js      petla_w_petli
        mov     edx, ecx                ; i = poprzedni

petla_zakonczeni:
        inc     edx                     ; ++i;
        cmp     edx, edi                ; while (i < n)
        jb      odwroc_petla

end:                                    ; return true
        mov     eax, 0x1
        ret

; tablica nie zawiera poprawnej permutacji. Musze przywrocic ja do stanu
; poczatkowego
popraw:
        dec     edx

popraw_petla:
        mov     eax, DWORD  [rsi+rdx*4] ; eax = p[i]
        and     eax, r9d                ; pozbywam sie oznaczenia
        and     DWORD  [rsi+rax*4], r9d ; pozbywam sie oznaczenia w p[p[i]]
        dec     edx                     ; --i
        test    edx, edx                ; if (i >= 0)
        jns     popraw_petla

bad:                                    ; return false
        xor     eax, eax
        ret