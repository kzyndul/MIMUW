package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy wierszowej.
 */
public class Wierszowa extends JednaTablica {

    Wierszowa (int wiersze, double[] a) {
        super(wiersze, a.length, a);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        return wartości[column];
    }

    @Override
    public IDoubleMatrix plus (double scalar) {
        double[] pomoc = dodaj_do_tablicy(wartości, scalar);
        return new Wierszowa(wymiary.rows, pomoc);
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        double[] pomoc = pomnóż_tablice(wartości, scalar);
        return new Wierszowa(wymiary.rows, pomoc);
    }

    @Override
    public double normInfinity () {
        double największy = 0;
        for (int i = 0; i < wartości.length; ++i) {
            if (Math.abs(wartości[i]) > największy) {
                największy = Math.abs(wartości[i]);
            }
        }
        return największy * wymiary.rows;
    }

    @Override
    public double normOne () {
        double suma = 0;
        for (int i = 0; i < wartości.length; ++i) {
            suma += Math.abs(wartości[i]);
        }
        return suma;
    }

    @Override
    public double frobeniusNorm () {
        double suma = 0;
        for (int i = 0; i < wartości.length; ++i) {
            suma += (wymiary.rows * Math.pow(wartości[i], 2));
        }
        return Math.sqrt(suma);
    }
}
