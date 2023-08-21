package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy kolumnowej
 */
public class Kolumnowa extends JednaTablica {

    Kolumnowa (int kolumny, double[] a) {
        super(a.length, kolumny, a);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        return wartości[row];
    }

    @Override
    public IDoubleMatrix plus (double scalar) {
        double[] pomoc = dodaj_do_tablicy(wartości, scalar);
        return new Kolumnowa(wymiary.columns, pomoc);
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        double[] pomoc = pomnóż_tablice(wartości, scalar);
        return new Kolumnowa(wymiary.columns, pomoc);
    }

    @Override
    public String toString () {
        if (wymiary.columns >= 3) {
            StringBuilder sb = new StringBuilder();
            sb.append(podaj_wymiar());
            for (int i = 0; i < wymiary.rows; ++i) {
                sb.append(wartości[i] + " ... " + wartości[i] + "\n");
            }
            return sb.toString();
        } else {
            return super.toString();
        }
    }

    @Override
    public double normInfinity () {
        double suma = 0;
        for (int i = 0; i < wartości.length; ++i) {
            if (suma < Math.abs(wartości[i])) {
                suma = Math.abs(wartości[i]);
            }
        }
        return suma * wymiary.columns;
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
            suma += (wymiary.columns * Math.pow(wartości[i], 2));
        }
        return Math.sqrt(suma);
    }
}
