#include "../day06/ReportBuilder.hpp"

#include <iostream>
#include <fstream>
#include <streambuf>
#include <vector>
#include <list>
#include <memory>
#include <algorithm>
#include <numeric>
#include <map>

#define SIZE 2000

class Map
    : public IScoreCalculator
{
public:
    Map(const std::string& aFileName)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            iMotions.push_back(line);
        }
    }

    void Snake(int aLength)
    {
        int size = SIZE; // iMotions.size();

        iTailMap.resize(size * size);
        std::fill(iTailMap.begin(), iTailMap.end(), 0);

        iStart = size * (size - size / 4) + size / 4;
        iSnake.resize(aLength);
        std::fill(iSnake.begin(), iSnake.end(), iStart);

        std::for_each(iMotions.begin(), iMotions.end(),
            [this](const std::string& aMotion)
            {
                Move(aMotion); 
            });
    }

    void Move(const std::string& aMotion)
    {
        auto direction = aMotion[0];
        auto step = std::stoi(aMotion.substr(2));
        auto size = SIZE; // iMotions.size();

        switch (direction)
        {
            case 'R' : MoveHead(step, 1); break;
            case 'L' : MoveHead(step, -1); break;
            case 'U' : MoveHead(step, -size); break;
            case 'D' : MoveHead(step, size); break;
            default: break;
        }
    }

    void MoveHead(int aStep, int aDelta)
    {
        for (int i = 0; i < aStep; i++)
        {
            iSnake[0] += aDelta; // Move head
            for (int j = 1; j < iSnake.size(); j++)
            {
                MoveTail(iSnake[j - 1], iSnake[j]);
            }
            iTailMap[iSnake.back()] = 1;
        }
    }
    void MoveTail(int iHead, int& iTail)
    {
        auto size = SIZE; // iMotions.size();

        int iHeadX = iHead % size; int iHeadY = iHead / size;
        int iTailX = iTail % size; int iTailY = iTail / size;

        {
            int deltaX = (iHeadX - iTailX);
            int deltaY = (iHeadY - iTailY);
            if ((std::abs(deltaX) > 1) && (std::abs(deltaY) > 1))
            {
                if (deltaX > 0) iTailX++;
                else iTailX--;
                if (deltaY > 0) iTailY++;
                else iTailY--;
            }
            else if (std::abs(deltaX) > 1)
            {
                if (deltaX > 0) iTailX++;
                else iTailX--;
                iTailY = iHeadY;
            }
            else if (std::abs(deltaY) > 1)
            {
                if (deltaY > 0) iTailY++;
                else iTailY--;
                iTailX = iHeadX;
            }
        }

        iTail = iTailX + iTailY * size;
    }
private: // IScoreCalculator
    std::string Header() override
    {
        return "--- Day 9: Rope Bridge ---";
    }
    std::string  Task1Score() override
    {
        Snake(2);
        int counter = std::accumulate(iTailMap.begin(), iTailMap.end(), 0);
        return std::to_string(counter);
    }
    std::string  Task2Score() override
    {
        Snake(10);
        int counter = std::accumulate(iTailMap.begin(), iTailMap.end(), 0);
        return std::to_string(counter);
    }
private:
    std::list<std::string> iMotions;
    int iStart;
    std::vector<int> iSnake;
    std::vector<int> iTailMap;
};

//////////////////////////////////////////////////////////////////////////////

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Map map(argv[1]);
    ReportBuilder report(&map);

    return 0;
}
