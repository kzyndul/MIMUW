package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy diagonalnej.
 */
public class Diagonalna extends Skośna {

    Diagonalna (double[] wartości) {
        super(wartości);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        if (row == column) {
            return wartości[row];
        }
        return 0;
    }


    public IDoubleMatrix plus (Diagonalna other) {
        double[] wynik = dodaj_dwie_tablice(other.wartości);
        return new Diagonalna(wynik);
    }


    @Override
    public IDoubleMatrix plus (IDoubleMatrix other) {

        assert wymiary.rows == other.shape().rows;
        assert wymiary.columns == other.shape().columns;
        if (Diagonalna.class.isInstance(other)) {
            return plus((Diagonalna) other);
        }
        return super.plus(other);
    }


    public IDoubleMatrix times (Diagonalna other) {
        double[] wynik = pomnóż_przez_tablice(other.wartości);
        return new Diagonalna(wynik);
    }

    @Override
    public IDoubleMatrix times (IDoubleMatrix other) {
        assert wymiary.columns == other.shape().rows;

        if (Diagonalna.class.isInstance(other)) {
            return times((Diagonalna) other);
        }
        return super.times(other);
    }


    @Override
    public IDoubleMatrix times (double scalar) {
        double[] pomoc = pomnóż_tablice(wartości, scalar);
        return new Diagonalna(pomoc);
    }

    @Override
    public String toString () {
        if (wymiary.rows < 3) {
            return super.toString();
        } else {
            StringBuilder sb = new StringBuilder();
            sb.append(podaj_wymiar());
            for (int i = 0; i < wymiary.rows; ++i) {
                /**
                 * Sprawdzam czy jest odpowiedni dużo znaków zerami od początku wiersza.
                 */
                if (i > 2) {
                    sb.append(0 + " ... " + 0 + " ");
                } else {
                    for (int k = 0; k < i; ++k) {
                        sb.append(0 + " ");
                    }
                }
                sb.append(get(i, i));
                /**
                 * Sprawdzam czy jest odpowiedni dużo znaków zerami do końca wiersza.
                 */
                if (wymiary.rows - i > 3) {
                    sb.append(" " + 0 + " ... " + 0);
                } else {
                    for (int k = i + 1; k < wymiary.rows; ++k) {
                        sb.append(" " + 0);
                    }
                }
                sb.append("\n");
            }
            return sb.toString();
        }
    }
}
