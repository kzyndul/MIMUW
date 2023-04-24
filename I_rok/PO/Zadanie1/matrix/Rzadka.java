package pl.edu.mimuw.matrix;

import java.util.*;

/**
 * Implementacja macierzy rzadkich. Reprezentowana jako tablica wartości MatrixCellValue posortowanych rosnąco względem
 * wiersza, a potem kolumny.
 */
public class Rzadka extends Macierz {

    MatrixCellValue[] wartości;

    Rzadka (int a, int b, MatrixCellValue[] wartości) {
        super(a, b);
        Arrays.sort(wartości, new MatrixCellValueRows());
        this.wartości = wartości;
    }

    @Override
    public double get (int row, int column) {
        wymiary.assertInShape(row, column);
        int indeks = Arrays.binarySearch(wartości, new MatrixCellValue(row, column, 0), new MatrixCellValueRows());
        if (indeks >= 0) {
            return wartości[indeks].value;
        }
        return 0;
    }

    /**
     * Dodawanie dwóch macierzy rzadkich.
     * @param other - macierz dodawana.
     * @return Nowa macierz typu rzadkiego będąca icj sumą.
     */
    public IDoubleMatrix plus (Rzadka other) {
        MatrixCellValue[] wynik = new MatrixCellValue[wartości.length + other.wartości.length];

        /**
         * Tworze nową tablicę, która jest sklejeniem tablic reprezentujących wartości macierzy dodawanych.
         */
        for (int i = 0; i < wartości.length; ++i) {
            wynik[i] = new MatrixCellValue(wartości[i].row, wartości[i].column, wartości[i].value);
        }
        for (int i = wartości.length; i < wartości.length + other.wartości.length; ++i) {
            wynik[i] = new MatrixCellValue(other.wartości[i - wartości.length].row,
                    other.wartości[i - wartości.length].column, other.wartości[i - wartości.length].value);
        }

        Arrays.sort(wynik, new MatrixCellValueRows());
        MatrixCellValue poprzedni = wynik[0];
        int i = 1;
        /**
         * Sprawdzam czy dwa elementy obok siebie nie opisują tej same komórki. Jak tak to jeżeli ich suma jest równa 0
         * to usuwa oba, w przeciwnym przypadku usuwam oba i tworze nowe polę będące ich sumą.
         */
        while (i < wynik.length && wynik[i] != null) {
            if (wynik[i].row == poprzedni.row && wynik[i].column == poprzedni.column) {
                if (wynik[i].value == -poprzedni.value) {
                    wynik[i] = null;
                    wynik[i - 1] = null;
                    for (int j = i; j < wynik.length - 2; ++j) {
                        wynik[j] = wynik[j + 2];
                    }
                    wynik[wynik.length - 1] = null;
                    wynik[wynik.length - 2] = null;
                } else {
                    wynik[i - 1] = new MatrixCellValue(wynik[i].row, wynik[i].column,
                            wynik[i].value + wynik[i - 1].value);
                    wynik[i] = null;
                    for (int j = i; j < wynik.length - 1; ++j) {
                        wynik[j] = wynik[j + 1];
                    }
                    wynik[wynik.length - 1] = null;
                }
            }
            ++i;
            poprzedni = wynik[i - 1];
        }
        int n = 0;
        while (n < wynik.length && wynik[n] != null) {
            ++n;
        }

        wynik = Arrays.copyOf(wynik, n);

        if (n == 0) {
            return new Stała(wymiary.rows, wymiary.columns, 0);
        }

        return new Rzadka(wymiary.rows, wymiary.columns, wynik);
    }

    @Override
    public IDoubleMatrix plus (IDoubleMatrix other) {

        assert wymiary.rows == other.shape().rows;
        assert wymiary.columns == other.shape().columns;
        if (Rzadka.class.isInstance(other)) {
            return plus((Rzadka) other);
        }
        return super.plus(other);
    }

    public MatrixCellValue[] pomnóż (double liczba) {
        MatrixCellValue[] wynik = new MatrixCellValue[wartości.length];
        for (int i = 0; i < wartości.length; ++i) {
            wynik[i] = new MatrixCellValue(wartości[i].row, wartości[i].column, wartości[i].value * liczba);
        }
        return wynik;
    }

    @Override
    public IDoubleMatrix times (double scalar) {
        MatrixCellValue[] wynik = pomnóż(scalar);
        return new Rzadka(wymiary.rows, wymiary.columns, wynik);
    }

    @Override
    public double frobeniusNorm () {
        double suma = 0;
        for (int i = 0; i < wartości.length; ++i) {
            suma += Math.pow(wartości[i].value, 2);
        }
        return Math.sqrt(suma);
    }


    @Override
    public double normInfinity () {

        double suma = 0;
        double max = 0;
        int i = 0;
        int obecny;
        while (i < wartości.length) {
            obecny = wartości[i].row;
            while (i < wartości.length && wartości[i].row == obecny) {
                suma += Math.abs(wartości[i].value);
                ++i;
            }
            if (suma > max) {
                max = suma;
            }
            suma = 0;
        }
        return max;
    }

    @Override
    public double normOne () {
        Arrays.sort(wartości, new MatrixCellValueColumns());
        double suma = 0;
        double max = 0;
        int i = 0;
        int obecny;
        while (i < wartości.length) {
            obecny = wartości[i].column;
            while (i < wartości.length && wartości[i].column == obecny) {
                suma += Math.abs(wartości[i].value);
                ++i;
            }
            if (suma > max) {
                max = suma;
            }
            suma = 0;
        }
        Arrays.sort(wartości, new MatrixCellValueRows());
        return max;
    }

    /**
     * Porównywanie względem wierszy a pootem kolumn.
     */
    private class MatrixCellValueRows implements Comparator<MatrixCellValue> {

        public int compare (MatrixCellValue a, MatrixCellValue b) {
            if (a.row - b.row == 0) {
                return a.column - b.column;
            }
            return a.row - b.row;
        }
    }


    /**
     * Porównywanie względem kolumn a pootem wiersz.
     */
    private class MatrixCellValueColumns implements Comparator<MatrixCellValue> {

        public int compare (MatrixCellValue a, MatrixCellValue b) {
            if (a.column - b.column == 0) {
                return a.row - b.row;
            }
            return a.column - b.column;
        }
    }


    /**
     * Zwraca tablice zawierającą indeksy na których zmienia się numer wiersza który dany ciąg elementów tablicy
     * "wartości" reprezentuje. To jest pierwszy indeks dla którego numer wiersza jest inny niż w poprzednim ciągu.
     */
    public int[] tablica_indeksow_wierszy () {
        int size = 0;
        int wielkość = 0;
        int[] wynik = new int[size];
        int obecny = wartości[0].row;
        for (int i = 0; i < wartości.length; ++i) {
            if (wartości[i].row != obecny) {
                if (size == wielkość) {
                    wynik = Arrays.copyOf(wynik, size * 2 + 1);
                    size = 2 * size + 1;
                }
                wynik[wielkość] = i;
                obecny = wartości[i].row;
                ++wielkość;
            }
        }
        if (size == wielkość) {
            wynik = Arrays.copyOf(wynik, size * 2 + 1);
            size = 2 * size + 1;
        }
        wynik[wielkość] = wartości.length;
        ++wielkość;
        wynik = Arrays.copyOf(wynik, wielkość);
        return wynik;
    }

    /**
     * Zwraca tablice zawierającą indeksy na których zmienia się numer kolumny który dany ciąg elementów tablicy
     * "wartości" reprezentuje. To jest pierwszy indeks dla którego numer kolumny jest inny niż w poprzednim ciągu.
     */
    public int[] tablica_indeksow_kolumn () {
        int size = 0;
        int wielkość = 0;
        int[] wynik = new int[size];
        int obecny = wartości[0].column;
        for (int i = 0; i < wartości.length; ++i) {
            if (wartości[i].column != obecny) {
                if (size == wielkość) {
                    wynik = Arrays.copyOf(wynik, size * 2 + 1);
                    size = 2 * size + 1;
                }
                wynik[wielkość] = i;
                obecny = wartości[i].column;
                ++wielkość;
            }
        }
        if (size == wielkość) {
            wynik = Arrays.copyOf(wynik, size * 2 + 1);
            size = 2 * size + 1;
        }
        wynik[wielkość] = wartości.length;
        ++wielkość;
        wynik = Arrays.copyOf(wynik, wielkość);
        return wynik;
    }


    public IDoubleMatrix times (Rzadka other) {
        int size = 0;
        int wielkość = 0;
        double suma = 0;
        int wiersza_ograniczenie_dolne = 0;
        int kolumny_ograniczenie_dolne = 0;

        MatrixCellValue[] wynik = new MatrixCellValue[size];

        int[] wiersze = tablica_indeksow_wierszy();
        Arrays.sort(other.wartości, new MatrixCellValueColumns());
        int[] kolumny = other.tablica_indeksow_kolumn();

        /**
         * Przechodze po nie zerowych wierszach.
         */
        for (int i = 0; i < wiersze.length; ++i) {
            kolumny_ograniczenie_dolne = 0;
            /**
             * Przechodze po niezerowych kolumnach.
             */
            for (int j = 0; j < kolumny.length; ++j) {
                int w = wiersza_ograniczenie_dolne;
                int k = kolumny_ograniczenie_dolne;

                /**
                 * Obliczam iloczyn skalarny wiersza z kolumną. Jeżeli jest równy zero nic nie robie, w przeciwnym
                 * przypadku dodaje nowy element do tablicy wynikowej.
                 */
                while (w < wiersze[i] && k < kolumny[j]) {
                    if (wartości[w].column < other.wartości[k].row) {
                        ++w;
                    } else if (wartości[w].column > other.wartości[k].row) {
                        ++k;
                    } else {
                        suma += wartości[w].value * other.wartości[k].value;
                        ++w;
                        ++k;
                    }
                }
                if (suma != 0) {
                    if (wielkość == size) {
                        wynik = Arrays.copyOf(wynik, 2 * size + 1);
                        size = size * 2 + 1;
                    }
                    wynik[wielkość] = new MatrixCellValue(wartości[wiersza_ograniczenie_dolne].row,
                            other.wartości[kolumny_ograniczenie_dolne].column, suma);
                    ++wielkość;
                }
                suma = 0;
                kolumny_ograniczenie_dolne = kolumny[j];
            }
            wiersza_ograniczenie_dolne = wiersze[i];
        }
        Arrays.sort(other.wartości, new MatrixCellValueRows());

        wynik = Arrays.copyOf(wynik, wielkość);
        return new Rzadka(wymiary.rows, other.shape().columns, wynik);
    }

    @Override
    public IDoubleMatrix times (IDoubleMatrix other) {
        assert wymiary.columns == other.shape().rows;
        if (Rzadka.class.isInstance(other)) {
            return times((Rzadka) other);
        }
        return super.times(other);
    }
}
