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

struct RearrangementStep
{
    int iMove;
    int iFrom;
    int iTo;
public:
    RearrangementStep(const std::string& aStep)
    {
        iMove = std::stoi(aStep.substr(aStep.find("move") + 5, aStep.find("from") - 1));
        iFrom = std::stoi(aStep.substr(aStep.find("from") + 5, aStep.find("to") - 1)) - 1;
        iTo = std::stoi(aStep.substr(aStep.find("to") + 3)) - 1;
    }
};

//////////////////////////////////////////////////////////////////////////////
class StackOfCrates
{
public:
    StackOfCrates()
    {
#if 0
        iStack.push_back("ZN");
        iStack.push_back("MCD");
        iStack.push_back("P");
#else
        iStack.push_back("RPCDBG");
        iStack.push_back("HVG");
        iStack.push_back("NSQDJPM");
        iStack.push_back("PSLGDCNM");
        iStack.push_back("JBNCPFLS");
        iStack.push_back("QBDZVGTS");
        iStack.push_back("BZMHFTQ");
        iStack.push_back("CMDBF");
        iStack.push_back("FCQG");
#endif
    }

    void operator () (const RearrangementStep& aStep)
    {
        for (int idx = 0; idx < aStep.iMove; idx++)
        {
            iStack[aStep.iTo].push_back(iStack[aStep.iFrom].back());
            iStack[aStep.iFrom].pop_back();
        }
    }

    std::string Value()
    {
        std::string result("");
        for (std::string stack : iStack)
        {
            result += stack.back();
        }
        return result;
    }
private:
    std::vector<std::string> iStack;
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
            iStacks(RearrangementStep(line));
        }
    }
private: // IScoreCalculator
    std::string Header() override
    {
        return "--- Day 5: Supply Stacks ---";
    }
    std::string  Task1Score() override
    {
        return iStacks.Value();
    }
    std::string  Task2Score() override
    {
        return "None";
    }
private:
    StackOfCrates iStacks;
};

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Expedition expedition(argv[1]);
    ReportBuilder report(&expedition);

    return 0;
}
