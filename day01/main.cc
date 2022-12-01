#include <iostream>
#include <fstream>
#include <streambuf>
#include <vector>
#include <list>
#include <memory>
#include <algorithm>
#include <numeric>

class Elf
{
public:
    void PutIntoBag(const std::string& aFood)
    {
        if (!aFood.empty())
        {
            iBag.push_back(aFood);
        }
    }
    int TotalCalories() const
    {   
        int calories = 0;
        for (auto const& food : iBag)
        {
            calories += std::stoi(food);
        }
        return calories;
    }
private:
    std::list<std::string> iBag;
};

class IInventory
{
public:
    virtual ~IInventory() = default;
    virtual int GetCalories(int aTopNUmber) = 0;
};

class Expedition
    : public IInventory
{
public:
    Expedition(const std::string& aFileName)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            AddItem(line);
        }
    }
    void AddItem(const std::string& anItem)
    {
        if (iSquad.empty() || anItem.empty())
        {
            iSquad.push_back(Elf());
        }
        iSquad.back().PutIntoBag(anItem);
    }
private: // IInventory
    int GetCalories(int aTopNUmber) override
    {
        std::sort(iSquad.begin(), iSquad.end(), 
                [](const Elf& lhs, const Elf& rhs)
                {
                    return lhs.TotalCalories() > rhs.TotalCalories();
                });

        return std::accumulate(iSquad.begin(), iSquad.begin() + aTopNUmber, 0,
                [](int lhs, const Elf& rhs)
                {
                    return lhs + rhs.TotalCalories();
                });
    }
private:
    std::vector<Elf> iSquad;
};

class ReportBuilder
{
public:
    ReportBuilder(IInventory *anInventory)
    {
        std::cout << "--- Day 1: Calorie Counting ---" << std::endl;
        std::cout << "Most calories:" << anInventory->GetCalories(1) << std::endl;
        std::cout << "Top three sum:" << anInventory->GetCalories(3) << std::endl;
    }
};

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Expedition expedition(argv[1]);
    ReportBuilder report(&expedition);

    return 0;
}
