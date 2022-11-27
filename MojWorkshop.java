package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;

import java.util.Collection;
import java.util.concurrent.Semaphore;

// JDDJD
public class MojWorkshop implements Workshop {

    private static int czekajacy = 0;
    private final Collection<Workplace> stanowiska;
    private Semaphore[] drugaOsoba;
    private Semaphore[] poUse;
    private long[] zajmowanePrzez;
    private final int N;

    public MojWorkshop (Collection<Workplace> stanowiska)
    {
        assert (stanowiska.size() != 0) : "pusta kolekcja";
        this.stanowiska = stanowiska;
        N = 2 * stanowiska.size();
        drugaOsoba = new Semaphore[N / 2];
        poUse = new Semaphore[N / 2];
        for (int i = 0; i < N / 2; ++i)
        {
            drugaOsoba[i] = new Semaphore(2, true);
            poUse[i] = new Semaphore(1, true);
        }
        zajmowanePrzez = new long[N / 2];
    }

    @Override
    public Workplace enter (WorkplaceId wid)
    {
        Workplace wynik = znajdzStanowisko(wid);
        int i = znajdzIndeks(wid);
        try
        {
            poUse[i].acquire();
            drugaOsoba[i].acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }

        zajmowanePrzez[i] = Thread.currentThread().getId();
        return wynik;
    }

    @Override
    public Workplace switchTo (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();
        int i = znajdzWatek(help);
        zajmowanePrzez[i] = 0;
        poUse[i].release();
        int j = znajdzIndeks(wid);
        try
        {
            poUse[j].acquire();
            drugaOsoba[j].acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        Workplace wynik = znajdzStanowisko(wid);
        zajmowanePrzez[j] = help;
        drugaOsoba[i].release();
        return wynik;
    }

    @Override
    public void leave ()
    {
        long help = Thread.currentThread().getId();
        int i = znajdzWatek(help);
        zajmowanePrzez[i] = 0;
        poUse[i].release();
        drugaOsoba[i].release();
    }

    private int znajdzIndeks (WorkplaceId wid)
    {
        int i = 0;
        for (Workplace stanowisko : stanowiska)
        {
            if (stanowisko.getId().equals(wid))
            {
                return i;
            }
            ++i;
        }
        return -1;
    }

    private Workplace znajdzStanowisko (WorkplaceId wid)
    {
        for (Workplace stanowisko : stanowiska)
        {
            if (stanowisko.getId().equals(wid))
            {
                return stanowisko;
            }
        }
        return null;
    }

    private int znajdzWatek (long id)
    {
        int i = 0;
        for (long kto : zajmowanePrzez)
        {
            if (kto == id)
            {
                return i;
            }
            ++i;
        }
        return -1;
    }

}
