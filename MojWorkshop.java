package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.Semaphore;

public class MojWorkshop implements Workshop {

    private ArrayList<MojWorkplace> stanowiska = new ArrayList<>();
    private ArrayList<Integer> czekajacy = new ArrayList<>();

    private Semaphore[] poUse;
    private Semaphore[] uzywam;
    private Semaphore[] swap;

    private long[] zajmowanePrzez;
    private long[] chcePracowac;
    private int[] swapNa;

    private final int N;
    private Semaphore czeka;


    public MojWorkshop (Collection<Workplace> stanowiska)
    {
        assert (stanowiska.size() != 0) : "pusta kolekcja";
        for (Workplace workplace : stanowiska)
        {
            MojWorkplace temp = new MojWorkplace(workplace, this);
            this.stanowiska.add(temp);
        }


        N = stanowiska.size();
        poUse = new Semaphore[N];
        swap = new Semaphore[N];
        uzywam = new Semaphore[N];
        zajmowanePrzez = new long[N];
        chcePracowac = new long[N];


        for (int i = 0; i < N; ++i)
        {
            poUse[i] = new Semaphore(1, true);
            uzywam[i] = new Semaphore(1, true);
            swap[i] = new Semaphore(1, true);
            zajmowanePrzez[i] = -1;
            chcePracowac[i] = -1;
        }

        czeka = new Semaphore(2 * N,true);
    }

    public void setZajmowanePrzez (int i, long a)
    {
        if (i != -1)
        {
            zajmowanePrzez[i] = a;
        }
    }

    public void setChcePracowac (int i, long a)
    {
        if (i != -1)
        {
            chcePracowac[i] = a;
        }
    }

    public void setUzywam (int i, int n)
    {
        if (i != -1)
        {
            if (n == 0)
            {
                try
                {
                    System.out.println(Thread.currentThread().getId() + " czekam tu na " + i);
                    uzywam[i].acquire();
                    System.out.println(Thread.currentThread().getId() + " juz nie czekam na " + i);
                }
                catch (InterruptedException e)
                {
                    throw new RuntimeException(e);
                }
            }
            else
            {
                uzywam[i].release();
            }
        }
    }




    @Override
    public Workplace enter (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();
        Workplace wynik = znajdzStanowisko(wid);
        int i = znajdzIndeks(wid);
        try
        {

            System.out.println(Thread.currentThread().getId() + " czekam w enter na " + i);
            poUse[i].acquire();
            System.out.println(Thread.currentThread().getId() + " juz nie czkam w enter na " + i);
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        chcePracowac[i] = help;
        return wynik;
    }

    @Override
    public Workplace switchTo (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();
        int i = znajdzWatek(help);
        int j = znajdzIndeks(wid);

        if (i == j && i != -1)
        {
            chcePracowac[j] = help;
            return znajdzStanowisko(wid);
        }

        poUse[i].release();
        try
        {
                        System.out.println(Thread.currentThread().getId() + " czekam w switchu na " + j);
            poUse[j].acquire();
            System.out.println(Thread.currentThread().getId() + " juz nie czkam w switchu na " + j);

        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        chcePracowac[j] = help;
        Workplace wynik = znajdzStanowisko(wid);
        return wynik;
    }

    @Override
    public void leave ()
    {
        long help = Thread.currentThread().getId();
        int i = znajdzWatek(help);
        System.out.println(Thread.currentThread().getId() + " opuszczam " + i);
        zajmowanePrzez[i] = -1;
        poUse[i].release();
        uzywam[i].release();
    }

    public int znajdzIndeks (WorkplaceId wid)
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

    public Workplace znajdzStanowisko (WorkplaceId wid)
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

    public int znajdzWatek (long id)
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

    public int znajdzGdzie (long id)
    {
        int i = 0;
        for (long kto : chcePracowac)
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
