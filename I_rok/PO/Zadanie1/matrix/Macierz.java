package pl.edu.mimuw.matrix;

/**
 * Klasa zawierająca uniwersalna implementację wszytkich metod z wyjatkiem get.
 */
public abstract class Macierz implements IDoubleMatrix {
    protected Shape wymiary;

    Macierz (int a, int b) {
        wymiary = Shape.matrix(a, b);
    }

    @Override
    public IDoubleMatrix plus (IDoubleMatrix other) {
        assert wymiary.equals(other.shape());
        double[][] pomoc = new double[wymiary.rows][wymiary.columns];
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                pomoc[i][j] = this.get(i, j) + other.get(i, j);
            }
        }
        return new Pełna(wymiary.rows, wymiary.columns, pomoc);
    }

    @Override
    public IDoubleMatrix minus (IDoubleMatrix other) {
        IDoubleMatrix pomoc = other.times(-1);
        return this.plus(pomoc);
    }

    @Override
    public IDoubleMatrix times (IDoubleMatrix other) {
        assert wymiary.columns == other.shape().rows;
        double[][] pomoc = new double[wymiary.rows][other.shape().columns];
        double suma = 0;
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < other.shape().columns; ++j) {
                for (int k = 0; k < wymiary.columns; ++k) {
                    suma += get(i, k) * other.get(k, j);
                }
                pomoc[i][j] = suma;
                suma = 0;
            }
        }
        return new Pełna(wymiary.rows, other.shape().columns, pomoc);
    }

    @Override
    public double[][] data () {
        double[][] wynik = new double[wymiary.rows][wymiary.columns];
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                wynik[i][j] = get(i, j);
            }
        }
        return wynik;
    }

    @Override
    public Shape shape () {
        return wymiary;
    }

    public String podaj_wymiar () {
        return ("Wiersze = " + wymiary.rows + " " + "Kolumny = " + wymiary.columns + "\n");
    }

    @Override
    public String toString () {
        String a = "";
        StringBuilder sa = new StringBuilder(a);
        sa.append(podaj_wymiar());
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                sa.append(" \t" + get(i, j));
            }
            sa.append("\n");
        }
        a = sa.toString();
        return a;
    }

    @Override
    public IDoubleMatrix plus (double scalar) {
        double[][] pomoc = new double[wymiary.rows][wymiary.columns];
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                pomoc[i][j] = get(i, j) + scalar;
            }
        }
        return new Pełna(wymiary.rows, wymiary.columns, pomoc);
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        double[][] pomoc = new double[wymiary.rows][wymiary.columns];
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                pomoc[i][j] = get(i, j) * scalar;
            }
        }
        return new Pełna(wymiary.rows, wymiary.columns, pomoc);
    }

    @Override
    public IDoubleMatrix minus (double scalar) {
        return plus(-scalar);
    }

    @Override
    public double normOne () {
        double sprawdzany = 0;
        double największy = 0;
        for (int j = 0; j < wymiary.columns; ++j) {
            for (int i = 0; i < wymiary.rows; ++i) {
                sprawdzany += Math.abs(get(i, j));
            }
            if (sprawdzany > największy) {
                największy = sprawdzany;
            }
            sprawdzany = 0;
        }
        return największy;
    }

    @Override
    public double normInfinity () {
        double sprawdzany = 0;
        double największy = 0;
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                sprawdzany += Math.abs(get(i, j));
            }
            if (sprawdzany > największy) {
                największy = sprawdzany;
            }
            sprawdzany = 0;
        }
        return największy;
    }

    @Override
    public double frobeniusNorm () {
        double wynik = 0;
        for (int i = 0; i < wymiary.rows; ++i) {
            for (int j = 0; j < wymiary.columns; ++j) {
                wynik += Math.pow(get(i, j), 2);
            }
        }
        return Math.sqrt(wynik);
    }

}
