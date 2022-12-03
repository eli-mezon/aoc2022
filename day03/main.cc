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

struct Item 
{
    Item(char aValue) : iValue(aValue) { }
    Item() : Item(0) { }
    // Every item type can be converted to a priority:
    // Lowercase item types a through z have priorities 1 through 26.
    // Uppercase item types A through Z have priorities 27 through 52.
    int Priority() const
    {
        if (islower(iValue)) return 1 + (iValue - 'a');
        if (isupper(iValue)) return 27 + (iValue - 'A');
        return 0;
    }
private:
    char iValue;
};

//////////////////////////////////////////////////////////////////////////////

struct Compartment
{
    Compartment(const std::string& aContent)
        : iContent(aContent) { }

    Item operator () (const Compartment& aSecondCompartment) const
    {
        for(const char& item : iContent)
        {
            if (aSecondCompartment.HasItem(item))
            {
                return Item(item);
            }
        }
        return Item();
    }

    bool HasItem(char anItem) const
    {
        return (iContent.find(anItem) != std::string::npos);
    }
private:
    std::string iContent;
};

//////////////////////////////////////////////////////////////////////////////

struct Rucksack
{
    std::string iContent;

    int RucksackScore() const
    {
        Compartment first(iContent.substr(0, iContent.length() / 2));
        Compartment second(iContent.substr(iContent.length() / 2));

        auto commonItem = first(second);

        return commonItem.Priority();
    }

    Item CommonItem(const Rucksack& aSecond, const Rucksack& aThird) const
    {
        for(const char& item : iContent)
        {
            if ((aSecond.HasItem(item)) && (aThird.HasItem(item)))
            {
                return Item(item);
            }
        }
        return Item();
    }

    bool HasItem(char anItem) const
    {
        return (iContent.find(anItem) != std::string::npos);
    }
};

//////////////////////////////////////////////////////////////////////////////

class IScoreCalculator
{
public:
    virtual ~IScoreCalculator() = default;
    virtual int Task1Score() = 0;
    virtual int Task2Score() = 0;
};

class Expedition
    : public IScoreCalculator
{
public:
    Expedition(const std::string& aFileName)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            iRucksacks.push_back(Rucksack{line});
        }
    }
private: // IScoreCalculator
    int Task1Score() override
    {
        return std::accumulate(iRucksacks.begin(), iRucksacks.end(), 0,
                [](int lhs, const Rucksack& rhs)
                {
                    return lhs + rhs.RucksackScore();
                });
    }
    int Task2Score() override
    {
        std::vector<Item> bages;

        for (std::vector<Rucksack>::iterator it = iRucksacks.begin(); it != iRucksacks.end();)
        {
            std::vector<Rucksack>::const_iterator first = it++;
            std::vector<Rucksack>::const_iterator second = it++;
            std::vector<Rucksack>::const_iterator third = it++;

            bages.push_back((*first).CommonItem(*second,*third));
        }

        return std::accumulate(bages.begin(), bages.end(), 0,
                [](int lhs, const Item& rhs)
                {
                    return lhs + rhs.Priority();
                });
    }
private:
    std::vector<Rucksack> iRucksacks;
};

class ReportBuilder
{
public:
    ReportBuilder(IScoreCalculator *aCalculator)
    {
        std::cout << "--- Day 3: Rucksack Reorganization ---" << std::endl;
        std::cout << "Total score (based on guess): " <<  aCalculator->Task1Score() << std::endl;
        std::cout << "Total score (based on strategy): " << aCalculator->Task2Score() << std::endl;
    }
};

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Expedition expedition(argv[1]);
    ReportBuilder report(&expedition);

    return 0;
}
