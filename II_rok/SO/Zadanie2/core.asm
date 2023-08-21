global core
extern get_value
extern put_value



section .data
mutex_tab:
    times N dq N                        ; Tablica przechowująca mutexy.




section .bss
przekazowa_tab:
    resq N                              ; Tablica na zamianę danych.



; rbx = n
; r12 = *p
section .text
core:
    push    rbp                         ; Zapamiętuję wartości rejestrów
    push    rbx                         ; rbp, rbx, r12
    push    r12
    mov     rbp, rsp
    
    mov     rbx, rdi                    ; Przenoszę wskaźnik p i numer rdzenia
    mov     r12, rsi                    ; na rejestry, które nie zmieniają
                                        ; wartości przy wywołaniu call.

    dec     r12                         ; Zmniejsza wartość wskaźnika ponieważ
                                        ; zawsze na początku go zwiększam.

; Główna pętla funkcji odpowiedzialna za wykonywanie odpowiednich instrukcji.
; Wczytuje charakter znajdujący się na obecnie rozpatrywanej pozycji po czym
; wykonuje odpowiednia akcję.
while:
    inc     r12                         ; Przechodzę do następnego znaku.
    xor     eax, eax
    mov     al, BYTE [r12]
    cmp     al, 0                       ; Sprawdzam czy nie jest zerem,
                                        ; oznaczało by to że jest to ostatni
                                        ; symbol tablicy p.

    jne     jmp_add

end:
    pop     rax

    lea     rsp, [rbp]                  ; W razie gdyby na stosie pozostały
    pop     r12                         ; jakieś śmieci usuwam je i przywracam
    pop     rbx                         ; rejestrom rbp, rbx, r12 ich wartości.
    pop     rbp

    ret



; Obsługa symbolu '+'.
jmp_add:
    cmp     al, '+'
    jne     jmp_mul

    pop     rax
    add     [rsp], rax
    jmp     while

; Obsługa symbolu '*'.
jmp_mul:
    cmp     al, '*'
    jne     jmp_sub

    pop     rax
    mul     QWORD [rsp]
    mov     [rsp], rax
    jmp     while

; Obsługa symbolu '-'.
jmp_sub:
    cmp     al, '-'
    jne     jmp_n

    neg     QWORD [rsp]
    jmp     while

; Obsługa symbolu 'n'.
jmp_n:
    cmp     al, 'n'
    jne     jmp_B

    push    rbx
    jmp     while


; Obsługa symbolu 'B'.
jmp_B:
    cmp     al, 'B'
    jne     jmp_C

    cmp     QWORD [rsp + 8], 0          ; Sprawdzam, czy druga wartość na
    je      B_0                         ; stosie jest zerem jeżeli to to nie
                                        ; przemieszczam sie po tablicy, w
    add     r12, [rsp]                  ; przeciwnym przypadku przemieszczam
                                        ; się o wartość na czubku stosu.
B_0:
    pop     rax                         ; Popuję wartość ze stosu/
    jmp     while

; Zmniejszam rozmiar skoków.
jmp_extend:
    jmp     while

; Obsługa symbolu 'C'.
jmp_C:
    cmp     al, 'C'
    jne     jmp_D

    pop     rax
    jmp     jmp_extend

; Obsługa symbolu 'D'.
jmp_D:
    cmp     al, 'D'
    jne     jmp_E

    mov     rax, [rsp]
    push    rax
    jmp     jmp_extend

; Obsługa symbolu 'E'.
jmp_E:
    cmp     al, 'E'
    jne     jmp_G

    pop     rax
    pop     rsi
    push    rax
    push    rsi
    jmp     jmp_extend

; Obsługa symbolu 'G'. Wstawiam argumenty do odpowiednich rejestrów, jeżeli
; stos jest dobrze wyrównany to wołam call, w przeciwnym przypadku wyrównuję
; stos i wołam call. Wstawiam na wierzchołek stosu wartość otrzymaną
; z wywołania funkcji.
jmp_G:
    cmp     al, 'G'
    jne     jmp_P

    mov     rdi, rbx                    ; Wstawiam argumenty do odpowiednich
    mov     rax, rbp                    ; rejestrów.
    sub     rax, rsp                    ; Sprawdzam czy stos jest dobrze
    test    rax, 8                      ; wyrównany. Jeżeli nie to wyrównuję.
    jz      G_good

    push    rax

    call    [rel get_value  wrt ..got]
    mov     [rsp], rax
    jmp     jmp_extend

G_good:

    call    [rel get_value  wrt ..got]
    push    rax
    jmp     jmp_extend

; Zapamietuje wynik anda
; Obsługa symbolu 'P'. Wstawiam argumenty do odpowiednich rejestrów, jeżeli
; stos jest dobrze wyrównany to wołam call, w przeciwnym przypadku wyrównuję
; stos i wołam call.
jmp_P:
    cmp     al, 'P'
    jne     jmp_S

    mov     rdi, rbx                    ; Wstawiam argumenty do odpowiednich
    mov     rsi, [rsp]                  ; rejestrów.

    mov     rax, rbp                    ; Sprawdzam czy stos jest dobrze
    sub     rax, rsp                    ; wyrównany. Jeżeli nie to wyrównuję.
    test    rax, 8
    jz      P_good

    pop     rax
    call    [rel put_value  wrt ..got]
    jmp     jmp_extend

; Zmniejszam rozmiar skoków.
jmp_extend_further:
    jmp     jmp_extend

P_good:
    call    [rel put_value  wrt ..got]
    pop     rax
    jmp     jmp_extend_further



; Obsługa symbolu 'S'. Czekam aż w tablicy z mutexami na pozycji n - numer
; rdzenia, będzie N. Wtedy wiem, że rdzeń dokonujący ostatnio ze mną zamiany
; już pobrał wartość. Wstawiam pod indeks n do tablicy na wymianę wartość z
; wierzchołka stosu. Informuję rdzeń, z którym mam się zamienić, że jestem
; gotowy, po czym zawieszam się na mutexie pod indeksem m - numer rdzenia,
; z którym się wymieniam. Czekam az na tym mutexie będzie wartość równa n.
; Wtedy wiem, że on też jest gotowy do wymiany i mogę kontynuować. Zabieram
; wartość z tablicy na wymiany z pod indeksu m i wstawiam ją na wierzchołek
; stosu. Następnie do tablicy mutexów pod indeks m wstawiam wartość N.
; Informując, że zakończyłem wymianę.
jmp_S:
    cmp     al, 'S'
    jne     jmp_numbers


    pop     rax                         ; rax = m numer drugiego rejestru.

    mov     rcx, [rsp]
    lea     rdi, [rel mutex_tab]        ; Ładuje adres tablicy mutexów.
    lea     rsi, [rel przekazowa_tab]   ; Ładuje adres tablicy na wymianę.

wait_S:                                 ; Czekam az u mnie w rejestrze będzie N.
    mov     rdx, [rdi + 8 * rbx]
    cmp     rdx, N
    jne     wait_S

    mov     [rsi + 8 * rbx], rcx        ; Przekazuję swoją wartość.
    mov     [rdi + 8 * rbx], rax        ; Powiadamiam parę, że jestem gotowy.

wait_SS:                                ; Czekam aż rdzeń m będzie gotowy.
    mov     rdx, [rdi + 8 * rax]
    cmp     rdx, rbx
    jne     wait_SS

    mov     rcx, [rsi + 8 * rax]        ; Biorę jego wartość.
    mov     [rsp], rcx
    mov     QWORD [rdi + 8 * rax], N    ; Mówię mu, że skończyłem

;S_e:
    jmp     jmp_extend_further


; Obsługa symboli '0 - 9'.
jmp_numbers:
;    movsx   rax, BYTE [r12]
    sub     rax, 48
    push    rax
    jmp     jmp_extend_further            ; loop again