#include "ReportBuilder.hpp"

#include <iostream>
#include <fstream>
#include <streambuf>
#include <vector>
#include <list>
#include <memory>
#include <algorithm>
#include <numeric>
#include <map>

//////////////////////////////////////////////////////////////////////////////

struct Section
{
    int iStart;
    int iEnd;

    Section(const std::string& anAssigment)
    {
        iStart = std::stoi(anAssigment.substr(0, anAssigment.find("-")));
        iEnd = std::stoi(anAssigment.substr(anAssigment.find("-") + 1));
    }

    // one of their assignments fully contains the other
    bool operator [] (const Section& aSection) const
    {
        return ((iStart <= aSection.iStart) && (iEnd >= aSection.iEnd));
    }

    // assignment pairs have the ranges overlap
    bool operator () (const Section& aSection) const
    {
        return ((iStart >= aSection.iStart) && (iStart <= aSection.iEnd));
    }

};

//////////////////////////////////////////////////////////////////////////////

struct PairOfElves
{
    std::string iAssignment;

    bool FullyContainRange() const
    {
        Section first(iAssignment.substr(0, iAssignment.find(",")));
        Section second(iAssignment.substr(iAssignment.find(",") + 1));

        return first[second] || second[first];
    }

    bool OverlapRange() const
    {
        Section first(iAssignment.substr(0, iAssignment.find(",")));
        Section second(iAssignment.substr(iAssignment.find(",") + 1));

        return first(second) || second(first);
    }
};

//////////////////////////////////////////////////////////////////////////////

class Expedition
    : public IScoreCalculator
{
public:
    Expedition(const std::string& aFileName)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            iRucksacks.push_back(PairOfElves{line});
        }
    }
private: // IScoreCalculator
    std::string Header() override
    {
        return "";
    }
    int Task1Score() override
    {
        return std::accumulate(iRucksacks.begin(), iRucksacks.end(), 0,
                [](int lhs, const PairOfElves& rhs)
                {
                    return lhs + rhs.FullyContainRange();
                });
    }
    int Task2Score() override
    {
        return std::accumulate(iRucksacks.begin(), iRucksacks.end(), 0,
                [](int lhs, const PairOfElves& rhs)
                {
                    return lhs + rhs.OverlapRange();
                });
    }
private:
    std::vector<PairOfElves> iRucksacks;
};

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Expedition expedition(argv[1]);
    ReportBuilder report(&expedition);

    return 0;
}
