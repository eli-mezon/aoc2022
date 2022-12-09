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


class Map
    : public IScoreCalculator
{
public:
    Map(const std::string& aFileName)
        : iSize(0)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            iHeights += line;
            iSize += 1;
        }
    }

    bool CanSeeNorthEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        if (y == 0) return true;
        bool result = true;
        for (int idx = x; idx < aPosition; idx += iSize)
        {
            result = result && (iHeights[idx] < height);
        }
        return result;
    }
    bool CanSeeSouthEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        if (y == iSize - 1) return true;
        bool result = true;
        for (int idx = aPosition + iSize; idx < iSize * iSize; idx += iSize)
        {
            result = result && (iHeights[idx] < height);
        }
        return result;
    }
    bool CanSeeEastEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        if (x == iSize - 1) return true;
        bool result = true;
        for (int idx = aPosition + 1; idx < (y + 1) * iSize; idx++)
        {
            result = result && (iHeights[idx] < height);
        }
        return result;
    }
    bool CanSeeWestEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        if (x == 0) return true;
        bool result = true;
        for (int idx = y * iSize; idx < aPosition; idx++)
        {
            result = result && (iHeights[idx] < height);
        }
        return result;
    }
    bool IsVisible(int aPosition)
    {
        return CanSeeNorthEdge(aPosition)
            || CanSeeSouthEdge(aPosition)
            || CanSeeEastEdge(aPosition)
            || CanSeeWestEdge(aPosition);
    }

    int ScoreNorthEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        int result = 0;
        for (int idx = aPosition - iSize; idx >= 0; idx -= iSize)
        {
            result += 1;
            if (iHeights[idx] >= height) break;
        }
        std::cout << result << ",";
        return result;
    }
    int ScoreSouthEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        int result = 0;
        for (int idx = aPosition + iSize; idx < iSize * iSize; idx += iSize)
        {
            result += 1;
            if (iHeights[idx] >= height) break;
        }
        std::cout << result << ",";
        return result;
    }
    int ScoreEastEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        int result = 0;
        for (int idx = aPosition + 1; idx < (y + 1) * iSize; idx++)
        {
            result += 1;
            if (iHeights[idx] >= height) break;
        }
        std::cout << result << ",";
        return result;
    }
    int ScoreWestEdge(int aPosition)
    {
        char height = iHeights[aPosition];
        int x = aPosition % iSize;
        int y = aPosition / iSize;

        int result = 0;
        for (int idx = aPosition - 1; idx > y * iSize - 1; idx--)
        {
            result += 1;
            if (iHeights[idx] >= height) break;
        }
        std::cout << result << ",";
        return result;
    }
    int ScenicScore(int aPosition)
    {
        int result = ScoreNorthEdge(aPosition)
            * ScoreEastEdge(aPosition)
            * ScoreSouthEdge(aPosition)
            * ScoreWestEdge(aPosition);
        std::cout << " = " << result << std::endl;
        return result;
    }
private: // IScoreCalculator
    std::string Header() override
    {
        return "--- Day 8: Treetop Tree House ---";
    }
    std::string  Task1Score() override
    {
        int counter = 0;
        for(int i = 0; i < iHeights.length(); i++)
        {
            counter += IsVisible(i);
        }
        return std::to_string(counter);
    }
    std::string  Task2Score() override
    {
        int max = 0;
        for(int i = 0; i < iHeights.length(); i++)
        {
            int score = ScenicScore(i);
            max = (score > max) ? score : max;
        }
        return std::to_string(max);
    }
private:
    std::string iHeights;
    int iSize;
};

//////////////////////////////////////////////////////////////////////////////


int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Map map(argv[1]);
    ReportBuilder report(&map);

    return 0;
}
