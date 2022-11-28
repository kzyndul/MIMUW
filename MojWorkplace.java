package cp2022.solution;

import cp2022.base.Workplace;

public class MojWorkplace extends Workplace {
    private final MojWorkshop warsztat;
    private final Workplace orginal;

    protected MojWorkplace (Workplace workplace, MojWorkshop warsztat)
    {
        super(workplace.getId());
        this.warsztat = warsztat;
        orginal = workplace;
    }

    @Override
    public void use ()
    {
//        System.out.println("Moje USe");

        long help = Thread.currentThread().getId();
        int i = warsztat.znajdzWatek(help);

        warsztat.setZajmowanePrzez(i, -1);
        warsztat.setUzywam(i, 1);
        int j = warsztat.znajdzGdzie(help);

        warsztat.setChcePracowac(j, -1);
        warsztat.setUzywam(j, 0);

        warsztat.setZajmowanePrzez(j, help);
        orginal.use();
    }
}
