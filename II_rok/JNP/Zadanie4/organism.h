#ifndef ZADANI4_ORGANISM_H
#define ZADANI4_ORGANISM_H

#include <concepts>
#include <cstdint>
#include <optional>
#include <tuple>

//template<typename T>
//concept Comparable = std::equality_comparable<T>;


template<typename species_t, bool can_eat_meat, bool can_eat_plants>
requires std::equality_comparable<species_t>
class Organism {
    const uint64_t vitality;
    const species_t species;

public:

    template<bool x_can_eat_meat, bool x_can_eat_plants>
    constexpr std::optional<Organism<species_t, can_eat_meat, can_eat_plants>>
    gody(Organism<species_t, x_can_eat_meat, x_can_eat_plants> x)
    {
        if (is_dead() || x.is_dead())
        {
            return std::nullopt;
        }
        return Organism<species_t, can_eat_meat, can_eat_plants>(species, (vitality + x.get_vitality()) / 2);
    }

    constexpr const bool isPlant() const {
        return !can_eat_meat && !can_eat_plants;
    }

    template<bool x_can_eat_meat, bool x_can_eat_plants>
    constexpr bool canIEat(Organism<species_t, x_can_eat_meat, x_can_eat_plants> x) {
        return ((can_eat_meat && !x.isPlant()) || (can_eat_plants && x.isPlant()) && !is_dead());
    }

    template<bool x_can_eat_meat, bool x_can_eat_plants>
    constexpr Organism
    bitwa(Organism<species_t, x_can_eat_meat, x_can_eat_plants> x)
    {

            if (canIEat(x))
            {
                if (x.isPlant())
                {
                    return {species, vitality + x.get_vitality()};
                }
                if (vitality > x.get_vitality())
                {
                    return {species, vitality + x.get_vitality() / 2};
                }
                if (x.canIEat(*this) && vitality <= x.get_vitality())
                {
                    return {species, 0};
                }
            }
            if (x.canIEat(*this))
            {
                if (isPlant())
                {
                    return {species, 0};
                }
                if (vitality < x.get_vitality())
                {
                    return {species, 0};
                }
            }
        return *this;
    }


    constexpr uint64_t get_vitality() const
    {
        return vitality;
    }

    constexpr const species_t &get_species() const
    {
        return species;
    }

    constexpr bool is_dead() const
    {
        return vitality == 0;
    }

    constexpr Organism(species_t const &species, uint64_t vitality) : vitality(
            vitality), species(species)
    {};
};


template <typename species_t, bool sp1_eats_m, bool sp1_eats_p, bool x_can_eat_meat, bool x_can_eat_plants, typename ... Args>
constexpr Organism<species_t, sp1_eats_m, sp1_eats_p>
help_encounter_series(Organism<species_t, sp1_eats_m, sp1_eats_p> organism, Organism<species_t, x_can_eat_meat, x_can_eat_plants> x, Args ... args) {
    auto wynik = get<0>(encounter(organism, x));
    if constexpr (sizeof...(args) > 0) {
        return help_encounter_series(wynik, args...);
    }
    return wynik;
}

template<typename species_t, bool sp1_eats_m, bool sp1_eats_p, bool sp2_eats_m, bool sp2_eats_p>
 std::tuple<Organism<species_t, sp1_eats_m, sp1_eats_p>,
        Organism<species_t, sp2_eats_m, sp2_eats_p>,
        std::optional<Organism<species_t, sp1_eats_m, sp1_eats_p>>>
 constexpr encounter(Organism<species_t, sp1_eats_m, sp1_eats_p> organism1,
          Organism<species_t, sp2_eats_m, sp2_eats_p> organism2)
{
    static_assert(!(sp1_eats_m == 0 && sp2_eats_p == 0 && sp2_eats_m == 0 && sp1_eats_p == 0));

    if (organism1.get_species() == organism2.get_species() && sp1_eats_m == sp2_eats_m && sp1_eats_p == sp2_eats_p) {
        return {organism1, organism2, organism1.gody(organism2)};
    }

    return std::make_tuple(organism1.bitwa(organism2), organism2.bitwa(organism1), std::nullopt);
}

template<typename specie_t> using Carnivore = Organism<specie_t, true, false>;
template<typename specie_t> using Omnivore = Organism<specie_t, true, true>;
template<typename specie_t> using Herbivore = Organism<specie_t, false, true>;
template<typename specie_t> using Plant = Organism<specie_t, false, false>;


template<typename species_t, bool sp1_eats_m, bool sp1_eats_p, typename ... Args>
constexpr Organism<species_t, sp1_eats_m, sp1_eats_p>
encounter_series(Organism<species_t, sp1_eats_m, sp1_eats_p> organism1,
                 Args ... args)
{
    if constexpr (sizeof ...(args) != 0)
    {
        return help_encounter_series(organism1, args...);
    }
    return organism1;
}


#endif