package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;

import java.util.Collection;
import java.util.HashMap;
import java.util.Queue;
import java.util.concurrent.Semaphore;

public class MojWorkshop implements Workshop {

    private HashMap<WorkplaceId, MojWorkplace> stanowiska_mapa;
    private HashMap<WorkplaceId, Integer> numerStanowiska;
    private HashMap<Long, WorkplaceId> zajmowanePrzez_mapa;
    private HashMap<Long, WorkplaceId> chcePracowac_mapa;

    private Semaphore[] poUse;
    private Semaphore[] uzywam;

    private final int N;

    private Queue<Long> czekam;


    public MojWorkshop (Collection<Workplace> stanowiska)
    {
        assert (stanowiska.size() != 0) : "pusta kolekcja";

        stanowiska_mapa = new HashMap<>();
        numerStanowiska = new HashMap<>();
        zajmowanePrzez_mapa = new HashMap<>();
        chcePracowac_mapa = new HashMap<>();

        N = stanowiska.size();
        poUse = new Semaphore[N];
        uzywam = new Semaphore[N];


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





    @Override
    public Workplace enter (WorkplaceId wid)
    {

        long help = Thread.currentThread().getId();

        Workplace wynik = stanowiska_mapa.get(wid);

        int i = znajdzIndeks_mapa(wid);
        try
        {

//            System.out.println(Thread.currentThread().getId() + " czekam w enter na " + i);
            poUse[i].acquire();
//            System.out.println(Thread.currentThread().getId() + " juz nie czkam w enter na " + i);
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
//                        System.out.println(Thread.currentThread().getId() + " czekam w switchu na " + j);
            poUse[j].acquire();
//            System.out.println(Thread.currentThread().getId() + " juz nie czkam w switchu na " + j);

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
