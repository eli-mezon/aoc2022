#include <string>
#include <iostream>

class IScoreCalculator
{
public:
    virtual ~IScoreCalculator() = default;
    virtual std::string Header() = 0;
    virtual std::string Task1Score() = 0;
    virtual std::string Task2Score() = 0;
};

class ReportBuilder
{
public:
    ReportBuilder(IScoreCalculator *aCalculator)
    {
        std::cout << aCalculator->Header() << std::endl;
        std::cout << "Task One score: " << aCalculator->Task1Score() << std::endl;
        std::cout << "Task Two score: " << aCalculator->Task2Score() << std::endl;
    }
};
