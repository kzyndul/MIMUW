package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy reprezentowanej jako tablica 2D.
 */
public class Pełna extends Macierz {
    protected double[][] wartości;

    Pełna (int a, int b, double[][] c) {
        super(a, b);
        assert c.length == a;
        for (int i = 0; i < c.length; ++i) {
            assert c[i].length == b;
        }
        assert c != null;
        wartości = c;
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        return wartości[row][column];
    }

}
