package pl.edu.mimuw;

import pl.edu.mimuw.matrix.*;

public class Main {
    public static void main (String[] args) {

        IDoubleMatrix diagonalna = DoubleMatrixFactory.diagonal(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
        System.out.print(diagonalna);


        IDoubleMatrix anty_diagonalna = DoubleMatrixFactory.antiDiagonal(0, 7, 6, 8, 9, 1, 2, 3, 4, 5);
        System.out.print(anty_diagonalna);


        IDoubleMatrix kolumnowa = DoubleMatrixFactory.kolumnowa(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0);
        System.out.print(kolumnowa);


        IDoubleMatrix wierszowa = DoubleMatrixFactory.wierszowa(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0);
        System.out.print(wierszowa);


        IDoubleMatrix identycznośc = DoubleMatrixFactory.identity(10);
        System.out.print(identycznośc);


        IDoubleMatrix rzdka = DoubleMatrixFactory.sparse(Shape.matrix(10, 10),
                new MatrixCellValue(2, 5, 10),
                new MatrixCellValue(5, 2, 15),
                new MatrixCellValue(0, 1, 11),
                new MatrixCellValue(7, 5, 12),
                new MatrixCellValue(0, 0, 19),
                new MatrixCellValue(9, 9, 17),
                new MatrixCellValue(0, 6, 18),
                new MatrixCellValue(1, 5, 18),
                new MatrixCellValue(3, 7, 10)
                );
        System.out.print(rzdka);

        IDoubleMatrix stała = DoubleMatrixFactory.stała(Shape.matrix(10, 10), 3);
        System.out.print(stała);

        IDoubleMatrix zero = DoubleMatrixFactory.zero(Shape.matrix(10, 10));
        System.out.print(zero);

        IDoubleMatrix wektor = DoubleMatrixFactory.vector(5, 3, 1, 2, 8, 9, 0, 7, 6, 4);
        System.out.print(wektor);

        IDoubleMatrix pełna = DoubleMatrixFactory.full(stworz_tablice());
        System.out.print(pełna);

    }

    public static double[][] stworz_tablice() {
        double[][] wynik = new double[10][10];
        for (int i = 0; i < 10; ++i)
        {
            for (int j = 0; j < 10; ++j)
            {
                wynik[i][j] = 10 + i + j;
            }
        }
        return wynik;
    }
}