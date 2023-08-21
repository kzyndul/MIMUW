package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy o wszystkich wartościach równych.
 */
public class Stała extends Macierz {

    protected double wartość;

    Stała (int a, int b, double c) {
        super(a, b);
        wartość = c;
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        return new Stała(wymiary.rows, wymiary.columns, wartość * scalar);
    }

    @Override
    public IDoubleMatrix plus (double scalar) {
        return new Stała(wymiary.rows, wymiary.columns, wartość + scalar);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        return wartość;
    }

    @Override
    public String toString () {
        if (wymiary.columns >= 3) {
            StringBuilder sb = new StringBuilder();
            sb.append(podaj_wymiar());
            for (int i = 0; i < wymiary.rows; ++i) {
                sb.append(wartość + " ... " + wartość + "\n");
            }
            return sb.toString();
        } else {
            return super.toString();
        }
    }

    @Override
    public double normOne () {
        return wymiary.rows * Math.abs(wartość);
    }

    @Override
    public double normInfinity () {
        return wymiary.columns * Math.abs(wartość);
    }

    @Override
    public double frobeniusNorm () {
        return Math.sqrt(Math.pow(wartość, 2) * wymiary.columns * wymiary.rows);
    }
}
