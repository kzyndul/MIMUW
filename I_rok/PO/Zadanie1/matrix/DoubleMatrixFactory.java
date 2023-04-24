package pl.edu.mimuw.matrix;

public class DoubleMatrixFactory {

    private DoubleMatrixFactory () {
    }

    public static IDoubleMatrix sparse (Shape shape, MatrixCellValue... values) {
        MatrixCellValue[] dane = values;
        assert dane != null;
        assert dane.length != 0;
        DoubleMatrixFactory.assert_sparse(shape, dane);
        return new Rzadka(shape.rows, shape.columns, dane);
    }

    /**
     * Sprawdza poprawność danych wejściowych. Czy któryś z elementów @param dane nie wykracza poza zakres macierzy.
     *
     * @param kształt - kształt macierzy .
     * @param dane    - tablica zawierająca wartości w poszczególnych komorkach.
     */
    public static void assert_sparse (Shape kształt, MatrixCellValue[] dane) {
        for (int i = 0; i < dane.length; ++i) {
            kształt.assertInShape(dane[i].row, dane[i].column);
        }
    }

    public static IDoubleMatrix full (double[][] values) {
        assert values != null;
        assert values.length != 0;
        return new Pełna(values.length, values[0].length, values);
    }

    /**
     * Tworzy tablice wypełnioną jedynkami rozmiaru @param size
     *
     * @param size - rozmiar tablicy.
     */
    public static double[] tablica_do_identycznosci (int size) {
        double[] wynik = new double[size];
        for (int i = 0; i < size; ++i) {
            wynik[i] = 1;
        }
        return wynik;
    }

    public static IDoubleMatrix identity (int size) {
        assert size != 0;
        double[] pomoc = tablica_do_identycznosci(size);
        return new Identyczność(pomoc);
    }

    public static IDoubleMatrix diagonal (double... diagonalValues) {
        double[] pomoc = diagonalValues;
        assert pomoc != null;
        assert pomoc.length != 0;
        return new Diagonalna(pomoc);
    }

    public static IDoubleMatrix antiDiagonal (double... antiDiagonalValues) {
        double[] pomoc = antiDiagonalValues;
        assert pomoc != null;
        assert pomoc.length != 0;
        return new AntyDiagonalna(pomoc);
    }

    public static IDoubleMatrix kolumnowa (int kolumny, double... columnsValues) {
        double[] pomoc = columnsValues;
        assert pomoc != null;
        assert pomoc.length != 0;
        return new Kolumnowa(kolumny, pomoc);
    }

    public static IDoubleMatrix wierszowa (int wiersze, double... RowsValues) {
        double[] pomoc = RowsValues;
        assert pomoc != null;
        assert pomoc.length != 0;
        return new Wierszowa(wiersze, pomoc);
    }

    public static IDoubleMatrix stała (Shape shape, double wartość) {
        return new Stała(shape.rows, shape.columns, wartość);
    }

    public static IDoubleMatrix vector (double... values) {
        double[] pomoc = values;
        assert pomoc != null;
        assert pomoc.length != 0;
        return new Wektor(pomoc);
    }

    public static IDoubleMatrix zero (Shape shape) {
        assert shape.columns >= 0;
        assert shape.rows >= 0;
        return new Zerowa(shape);
    }
}
