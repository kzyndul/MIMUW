package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy identyczniściowej. Przydatna przy optymalizacji dodawania i mnozenia macierzy.
 */
public class Identyczność extends Diagonalna {
    Identyczność(double[] wartości)
    {
        super(wartości);
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        if (row == column) {
            return 1;
        }
        return 0;
    }

    @Override
    public double normOne () {
        return 1;
    }

    @Override
    public double normInfinity () {
        return 1;
    }

    @Override
    public double frobeniusNorm () {
        return Math.sqrt(wymiary.rows);
    }
}
