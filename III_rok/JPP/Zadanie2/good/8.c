int ala(ref int x) {
    x = x + 1;
    if (x < 5) {
        int y = ala(ref x);
    };
    return x;
}


int main() {
    int a = 0;
    int tmp = ala(ref a);
    print(a);
    print("\n");
    return 0;
}
