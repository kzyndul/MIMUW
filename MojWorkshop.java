package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

public class MojWorkshop implements Workshop {

    private ConcurrentHashMap<WorkplaceId, MojWorkplace> stanowiska_mapa;
    private ConcurrentHashMap<WorkplaceId, Integer> numerStanowiska;
    private ConcurrentHashMap<Long, WorkplaceId> zajmowanePrzez_mapa;
    private ConcurrentHashMap<Long, WorkplaceId> chcePracowac_mapa;
    private ConcurrentHashMap<Long, WorkplaceId> wszedlemNa;
    private Set<Long> obecnyCykl = ConcurrentHashMap.newKeySet();


    private Semaphore[] swap;
    private Semaphore[] poUse;
    private Semaphore[] uzywam;

    private final int N;

    private LinkedList<Para<Long, Boolean>> czekajacy;
    private int ileCzeka = 0;
    Semaphore czekam = new Semaphore(0, true);
    Semaphore mutex_wejscie = new Semaphore(1, true);
    Semaphore mutex_wyjscie = new Semaphore(1, true);


    Semaphore mutex = new Semaphore(1, true);
    Semaphore rozCykl = new Semaphore(1, true);
//    int rozCyklInt = 1;



    public MojWorkshop (Collection<Workplace> stanowiska)
    {
        assert (stanowiska.size() != 0) : "pusta kolekcja";

        stanowiska_mapa = new ConcurrentHashMap<>();
        numerStanowiska = new ConcurrentHashMap<>();
        zajmowanePrzez_mapa = new ConcurrentHashMap<>();
        chcePracowac_mapa = new ConcurrentHashMap<>();
        wszedlemNa = new ConcurrentHashMap<>();

        N = stanowiska.size();
        poUse = new Semaphore[N];
        uzywam = new Semaphore[N];
        swap = new Semaphore[N];

        czekajacy = new LinkedList<Para<Long, Boolean>>();

        int k = 0;
        for (Workplace workplace : stanowiska)
        {
            MojWorkplace temp = new MojWorkplace(workplace, this);

            stanowiska_mapa.put(temp.getId(), temp);
            numerStanowiska.put(workplace.getId(), k);

            poUse[k] = new Semaphore(1, true);
            uzywam[k] = new Semaphore(1, true);
            swap[k] = new Semaphore(1, true);
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

    public void setChcePracowac_mapa (long help)
    {
        chcePracowac_mapa.remove(help);
    }

    public void zwolnijStary (long help)
    {
        WorkplaceId temp = zajmowanePrzez_mapa.get(help);
        if (temp != null)
        {
            int i = numerStanowiska.get(temp);
            zajmowanePrzez_mapa.remove(help); // dodalem
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

//    public void rozCykl (long thred)
//    {
//        obecnyCykl.remove(thred);
//        if (rozCyklInt == 0 && obecnyCykl.size() == 0)
//        {
//            rozCykl.release();
//            ++rozCyklInt;
//        }
//    }


    public void wyjdz ()
    {
        long help = Thread.currentThread().getId();
        int i = czekajacy.indexOf(new Para<>(help, true));
        int ii = czekajacy.indexOf(new Para<>(help, false));
        i = Math.max(i, ii);

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


    private Long getKeyByValue (WorkplaceId value)
    {

        for (Map.Entry<Long, WorkplaceId> entry : zajmowanePrzez_mapa.entrySet())
        {
            if (Objects.equals(value, entry.getValue()))
            {
                return entry.getKey();
            }
        }
        return null;
    }

    private Set<Long> cykl (long thred)
    {
        Set<Long> wynik = ConcurrentHashMap.newKeySet();
        wynik.add(thred);
        WorkplaceId p = chcePracowac_mapa.get(thred);
        WorkplaceId a = null;
        Long temp = null;
        while (true)
        {
            temp = getKeyByValue(p);
            if (temp == null || p.equals(a))
            {
                return ConcurrentHashMap.newKeySet();
            }
            if (wynik.contains(temp))
            {
                return wynik;
            }
            wynik.add(temp);
            a = p;
            p = chcePracowac_mapa.get(temp);
        }
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
            poUse[i].acquire();
            swap[i].acquire(); // dodalem
            mutex_wyjscie.acquire();
            wyjdz();
            mutex_wyjscie.release();

            rozCykl.acquire();
            chcePracowac_mapa.put(help, wid);
            rozCykl.release();



        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        wszedlemNa.put(help, wid); // dodalem
        return wynik;
    }

    @Override
    public Workplace switchTo (WorkplaceId wid)
    {
        long help = Thread.currentThread().getId();

        WorkplaceId temp = zajmowanePrzez_mapa.get(help);


        int i = numerStanowiska.get(temp);
        int j = numerStanowiska.get(wid);


        if (wid == temp)
        {
            try
            {
                rozCykl.acquire();
            }
            catch (InterruptedException e)
            {
                throw new RuntimeException(e);
            }
            chcePracowac_mapa.put(help, wid);
            rozCykl.release();
            return stanowiska_mapa.get(wid);
        }
        try
        {
            rozCykl.acquire();
            chcePracowac_mapa.put(help, wid);
            obecnyCykl.addAll(cykl(help));
            rozCykl.release();

            wejdz_swap();

            if (!obecnyCykl.isEmpty())
            {
                swap[i].release();
                swap[j].acquire();
            }
            else
            {
                swap[j].acquire();
                if (!obecnyCykl.isEmpty())
                {
                    if (!obecnyCykl.contains(help))
                    {
                        swap[j].release();
                        swap[j].acquire();
                    }
                    obecnyCykl.remove(help);
                }
                swap[i].release();
            }

            mutex_wyjscie.acquire();
            wyjdz();
            mutex_wyjscie.release();
        }
        catch (InterruptedException e)
        {
            throw new RuntimeException(e);
        }
        return stanowiska_mapa.get(wid);
    }

    @Override
    public void leave ()
    {
        long help = Thread.currentThread().getId();
        WorkplaceId temp = zajmowanePrzez_mapa.get(help);
        int i = numerStanowiska.get(temp);
        zajmowanePrzez_mapa.remove(help);
        swap[i].release();
        int j = numerStanowiska.get(wszedlemNa.get(help));
        poUse[j].release();
        uzywam[i].release();
    }
}
