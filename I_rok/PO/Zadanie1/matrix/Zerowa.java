package pl.edu.mimuw.matrix;

/**
 * Implementacja macierzy zerowej. Przydatna przy optymalizacji dodawania i mnozenia macierzy.
 */
public class Zerowa extends Stała {
    Zerowa(Shape shape)
    {
        super(shape.rows, shape.columns, 0);
    }

    @Override
    public double normInfinity () {
        return 0;
    }

    @Override
    public double normOne () {
        return 0;
    }

    @Override
    public double frobeniusNorm () {
        return 0;
    }
}
