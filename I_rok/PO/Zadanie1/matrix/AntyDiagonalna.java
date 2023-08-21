package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy antydiagonalnej.
 */
public class AntyDiagonalna extends Skośna {

    AntyDiagonalna (double[] wartości) {
        super(wartości);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        if (row + column == wymiary.columns - 1) {
            return wartości[row];
        }
        return 0;
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        double[] pomoc = pomnóż_tablice(wartości, scalar);
        return new AntyDiagonalna(pomoc);
    }

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
                if (wymiary.rows - i > 3) {
                    sb.append(0 + " ... " + 0 + " ");
                } else {
                    for (int k = i + 1; k < wymiary.rows; ++k) {
                        sb.append(0 + " ");
                    }
                }
                sb.append(get(i, wymiary.rows - i - 1));
                /**
                 * Sprawdzam czy jest odpowiedni dużo znaków zerami do końca wiersza.
                 */
                if (i > 2) {
                    sb.append(" " + 0 + " ... " + 0);
                } else {
                    for (int k = 0; k < i; ++k) {
                        sb.append(" " + 0);
                    }
                }
                sb.append("\n");
            }
            return sb.toString();
        }
    }
}
