package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

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
        long help = Thread.currentThread().getId();
        warsztat.zwolnijStary(help);

        warsztat.zajmij(orginal.getId());
//        warsztat.rozCykl(help);
        warsztat.setZajmowanePrzez_mapa(help, orginal.getId()); // niew powinnow byc
        warsztat.setChcePracowac_mapa(help);

        orginal.use();
    }
}
