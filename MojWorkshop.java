package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

// co 2 * N czekac
public class MojWorkshop implements Workshop {

    private ConcurrentHashMap<WorkplaceId, MojWorkplace> stanowiska_mapa;
    private ConcurrentHashMap<WorkplaceId, Integer> numerStanowiska;
    private ConcurrentHashMap<Long, WorkplaceId> zajmowanePrzez_mapa;
    private ConcurrentHashMap<Long, WorkplaceId> chcePracowac_mapa;

    private Semaphore[] poUse;
    private Semaphore[] uzywam;

    private final int N;

    private LinkedList<Para<Long, Boolean>> czekajacy;
    private int ileCzeka = 0;
    Semaphore czekam = new Semaphore(0, true);
    Semaphore mutex_wejscie = new Semaphore(1, true);
    Semaphore mutex_wyjscie = new Semaphore(1, true);

    Semaphore mutex = new Semaphore(1, true);




    public MojWorkshop (Collection<Workplace> stanowiska)
    {
        assert (stanowiska.size() != 0) : "pusta kolekcja";

        stanowiska_mapa = new ConcurrentHashMap<>();
        numerStanowiska = new ConcurrentHashMap<>();
        zajmowanePrzez_mapa = new ConcurrentHashMap<>();
        chcePracowac_mapa = new ConcurrentHashMap<>();

        N = stanowiska.size();
        poUse = new Semaphore[N];
        uzywam = new Semaphore[N];

        czekajacy = new LinkedList<Para<Long, Boolean>>();

        int k =0;
        for (Workplace workplace : stanowiska)
        {
            MojWorkplace temp = new MojWorkplace(workplace, this);

            stanowiska_mapa.put(temp.getId(), temp);
            numerStanowiska.put(workplace.getId(), k);

            poUse[k] = new Semaphore(1, true);
            uzywam[k] = new Semaphore(1, true);
            ++k;
        }
    }

    public int znajdzIndeks_mapa (WorkplaceId id)
    {
        if (id == null)
        {
            return -1;
        }
        return numerStanowiska.get(id);
    }


    public void setZajmowanePrzez_mapa (long help, WorkplaceId id)
    {
        if (id == null)
        {
            zajmowanePrzez_mapa.remove(help);
        }
        else
        {
            zajmowanePrzez_mapa.put(help, id);
        }
    }

    public WorkplaceId setChcePracowac_mapa (long help)
    {
        WorkplaceId temp = chcePracowac_mapa.get(help);
        chcePracowac_mapa.remove(help);
        return temp;
    }

    public void zwolnijStary (long help)
    {
        WorkplaceId temp = zajmowanePrzez_mapa.get(help);
        if (temp != null)
        {
            int i = numerStanowiska.get(temp);
            uzywam[i].release();
        }
    }

    public void zajmij (WorkplaceId id)
    {
        int i = numerStanowiska.get(id);
        try
        {
            uzywam[i].acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }

    }


    public void wejdz ()
    {

        long thred = Thread.currentThread().getId();
        if (ileCzeka == 2 * N)
        {
            try
            {
                czekam.acquire();
            }
            catch (InterruptedException e)
            {
                throw new RuntimeException(e);
            }
        }

//                    System.out.println(Thread.currentThread().getId() + " dodaje");

        try
        {
            mutex.acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        ++ileCzeka;
        czekajacy.add(new Para<>(thred, true));
        mutex.release();
    }

    public void wejdz_swap ()
    {
        long thred = Thread.currentThread().getId();
        try
        {
            mutex.acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        czekajacy.add(new Para<>(thred, false));
        mutex.release();

    }



    public void wyjdz()
    {
        long help = Thread.currentThread().getId();
        int i = czekajacy.indexOf(new Para<>(help, true));
        int ii = czekajacy.indexOf(new Para<>(help, false));
        i = Math.max(i, ii);

        if (i == -1)
        {
            System.out.println("JD");

        }
//        System.out.println(Thread.currentThread().getId() + " odejmje");
        try
        {
            mutex.acquire();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }

        if (!czekajacy.get(i).getDrugi() && i != 0)
        {
            czekajacy.remove(i);
        }
        else
        {
            czekajacy.set(i, new Para<>((long) 0, czekajacy.get(i).getDrugi()));
        }



        if (i == 0)
        {
            int temp = ileCzeka;
            int j = 0;
            while (!czekajacy.isEmpty() && czekajacy.peekFirst().getPierwszy() == 0)
            {
                if (czekajacy.peekFirst().getDrugi())
                {
                    ++j;
                }
                czekajacy.removeFirst();
            }
            ileCzeka = Math.max(0, ileCzeka - j);
            if (temp >= 2 * N)
            {
                czekam.release();
            }
        }
        mutex.release();
    }

    @Override
    public Workplace enter (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();
        Workplace wynik = stanowiska_mapa.get(wid);

        int i = znajdzIndeks_mapa(wid);
        try
        {

            mutex_wejscie.acquire();
            wejdz();
            mutex_wejscie.release();
//            System.out.println(Thread.currentThread().getId() + " czekam w enter na " + i);
            poUse[i].acquire();
//            System.out.println(Thread.currentThread().getId() + " juz nie czkam w enter na " + i);
            mutex_wyjscie.acquire();
            wyjdz();
            mutex_wyjscie.release();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        chcePracowac_mapa.put(help, wid);
        return wynik;
    }

    @Override
    public Workplace switchTo (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();

        // stare stanowisko
        WorkplaceId temp = zajmowanePrzez_mapa.get(help);


        int i = numerStanowiska.get(temp);
        int j = numerStanowiska.get(wid);


        if (wid == temp)
        {
            chcePracowac_mapa.put(help, wid);
            return stanowiska_mapa.get(wid);
        }

        poUse[i].release();
        try
        {
//            mutex_wejscie.acquire();
            wejdz_swap();
//            mutex_wejscie.release();
//                        System.out.println(Thread.currentThread().getId() + " czekam w switchu na " + j);
            poUse[j].acquire();
//            System.out.println(Thread.currentThread().getId() + " juz nie czkam w switchu na " + j);

            mutex_wyjscie.acquire();
            wyjdz();
            mutex_wyjscie.release();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        chcePracowac_mapa.put(help, wid);
        return stanowiska_mapa.get(wid);
    }

    @Override
    public void leave ()
    {
        long help = Thread.currentThread().getId();
        WorkplaceId temp = zajmowanePrzez_mapa.get(help);
        int i = numerStanowiska.get(temp);
//        System.out.println(Thread.currentThread().getId() + " opuszczam " + i);
        zajmowanePrzez_mapa.remove(help);
        poUse[i].release();
        uzywam[i].release();
    }
}
