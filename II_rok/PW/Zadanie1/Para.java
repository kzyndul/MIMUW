package cp2022.solution;

import java.util.Objects;

public class Para<T1, T2> {
    private T1 pierwszy;
    private T2 drugi;
    Para(T1 t1, T2 t2)
    {
        pierwszy = t1;
        drugi = t2;
    }

    public T1 getPierwszy ()
    {
        return pierwszy;
    }

    public T2 getDrugi ()
    {
        return drugi;
    }

    @Override
    public boolean equals (Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }
        Para<?, ?> para = (Para<?, ?>) o;
        return pierwszy.equals(para.pierwszy) && drugi.equals(para.drugi);
    }

    @Override
    public int hashCode ()
    {
        return Objects.hash(pierwszy, drugi);
    }

    public boolean isNULL ()
    {
        return this == null;
    }
}
