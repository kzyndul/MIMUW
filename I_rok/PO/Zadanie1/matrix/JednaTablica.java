package pl.edu.mimuw.matrix;

/**
 * Klasa zawierająca implementacje metod wspólnych dla wszystkich macierzy, których wartości są reprezentowane przez
 * jedną tablicę.
 */
public abstract class JednaTablica extends Macierz {

    protected double[] wartości;

    JednaTablica (int a, int b, double[] c) {
        super(a, b);
        wartości = c;
    }

    public static double[] pomnóż_tablice (double[] a, double b) {
        double[] wynik = new double[a.length];
        for (int i = 0; i < a.length; ++i) {
            wynik[i] = a[i] * b;
        }
        return wynik;
    }

    public static double[] dodaj_do_tablicy (double[] a, double b) {
        double[] wynik = new double[a.length];
        for (int i = 0; i < a.length; ++i) {
            wynik[i] = a[i] + b;
        }
        return wynik;
    }
}
