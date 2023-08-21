package pl.edu.mimuw.matrix;

/**
 * Klasa implementująca metody wspólne dla macierzy zawierających wartości tylko na przekątnych.
 */
public abstract class Skośna extends JednaTablica {

    Skośna (double[] wartości) {
        super(wartości.length, wartości.length, wartości);
    }

    @Override
    public double normOne () {
        double największy = 0;
        for (int i = 0; i < wartości.length; ++i) {
            if (Math.abs(wartości[i]) > największy) {
                największy = Math.abs(wartości[i]);
            }
        }
        return największy;
    }

    public double[] dodaj_dwie_tablice(double[] druga)
    {
        double[] wynik = new double[wartości.length];
        assert wartości.length == druga.length;
        for (int i = 0; i < wartości.length; ++i)
        {
            wynik[i] = wartości[i] + druga[i];
        }
        return wynik;
    }


    public double[] pomnóż_przez_tablice(double[] druga)
    {
        double[] wynik = new double[wartości.length];
        assert wartości.length == druga.length;
        for (int i = 0; i < wartości.length; ++i)
        {
            wynik[i] = wartości[i] * druga[i];
        }
        return wynik;
    }

    @Override
    public double normInfinity () {
        return normOne();
    }

    @Override
    public double frobeniusNorm () {
        double suma = 0;
        for (int i = 0; i < wartości.length; ++i) {
            suma += Math.pow(wartości[i], 2);
        }
        return Math.sqrt(suma);
    }
}
