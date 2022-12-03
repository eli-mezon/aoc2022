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

// score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
enum class HandShape { Rock = 1, Paper = 2, Scissors = 3 };

constexpr
bool operator > (HandShape lhs, HandShape rhs)
{
    switch (lhs)
    {
        case HandShape::Rock: return (HandShape::Scissors == rhs);
        case HandShape::Paper: return (HandShape::Rock == rhs);
        case HandShape::Scissors: return (HandShape::Paper == rhs);
    }
    return false;
}

constexpr
bool operator < (HandShape lhs, HandShape rhs)
{
    return (lhs != rhs) && !(lhs > rhs);
}

static_assert(HandShape::Rock < HandShape::Paper);      // Paper defeats Rock.
static_assert(HandShape::Paper < HandShape::Scissors);  // Scissors defeats Paper.
static_assert(HandShape::Scissors < HandShape::Rock);   // Rock defeats Scissors.

//////////////////////////////////////////////////////////////////////////////

struct Player
{
    HandShape iHandShape;

    Player(
        const std::string& aCode, 
        const std::map<std::string, HandShape>& aDecoder)

        : iHandShape(aDecoder.at(aCode)) { }

    Player(const std::string& aCode)
        : Player(aCode, iDefaultDecoder) { }

    // the score for the shape you selected
    int operator () () const
    {
        return static_cast<int>(iHandShape);
    }

    // the score for the outcome of the round
    int operator () (const Player& anOpponent) const
    {
        return (iHandShape != anOpponent.iHandShape)
            ? (iHandShape > anOpponent.iHandShape) ? WIN_SCORE : LOSE_SCORE
            : DRAW_SCORE;
    }
private:
    static std::map<std::string, HandShape> iDefaultDecoder;
    //(0 if you lost, 3 if the round was a draw, and 6 if you won).
    static constexpr int LOSE_SCORE = 0;
    static constexpr int DRAW_SCORE = 3;
    static constexpr int WIN_SCORE = 6;
};

std::map<std::string, HandShape> Player::iDefaultDecoder = 
{
    { "A", HandShape::Rock },
    { "B", HandShape::Paper },
    { "C", HandShape::Scissors },
};

//////////////////////////////////////////////////////////////////////////////

struct Round
{
    std::string iRoundRecord;

    int TotalScore(const Player& anOppenetHand, const Player& aMyHand) const
    {
        return aMyHand() + aMyHand(anOppenetHand);
    }
public:
    Player OpponentHand() const
    {
        return Player(iRoundRecord.substr(0, iRoundRecord.find(" ")));
    }
    Player MyHandFromGuess() const
    {
        return Player(iRoundRecord.substr(iRoundRecord.find(" ") + 1), iPrivateDecoder);
    }
    Player MyHardFromStrategy() const
    {
        auto opponentHandShape = OpponentHand().iHandShape;
        auto strategyTable = iStrategy.at(opponentHandShape);
        return Player(strategyTable.at(iRoundRecord.substr(iRoundRecord.find(" ") + 1)));
    }
private:
    static std::map<std::string,HandShape> iPrivateDecoder;
    static std::map<HandShape, std::map<std::string, std::string>> iStrategy;
};
// Part One
// The second column, you reason, must be what you should play in response:
// X for Rock, Y for Paper, and Z for Scissors.
std::map<std::string, HandShape> Round::iPrivateDecoder = 
{
    { "X", HandShape::Rock },
    { "Y", HandShape::Paper },
    { "Z", HandShape::Scissors },
};
// Part Two
// The second column says how the round needs to end: 
// X means you need to lose, Y means you need to end the round in a draw,
// and Z means you need to win.
std::map<HandShape, std::map<std::string, std::string>> Round::iStrategy =
{
    { HandShape::Rock, { { "X", "C" }, { "Y", "A" },  { "Z", "B" } } }, 
    { HandShape::Paper, { { "X", "A" }, { "Y", "B" },  { "Z", "C" } } }, 
    { HandShape::Scissors, { { "X", "B" }, { "Y", "C" },  { "Z", "A" } } },
};

//////////////////////////////////////////////////////////////////////////////

class IScoreCalculator
{
public:
    virtual ~IScoreCalculator() = default;
    virtual int Task1Score() = 0;
    virtual int Task2Score() = 0;
};

class Tournament
    : public IScoreCalculator
{
public:
    Tournament(const std::string& aFileName)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
            iEncryptedStrategyGuide.push_back(Round{line});
        }
    }
private: // IScoreCalculator
    int Task1Score() override
    {
        return std::accumulate(iEncryptedStrategyGuide.begin(), iEncryptedStrategyGuide.end(), 0,
                [](int lhs, const Round& rhs)
                {
                    return lhs + rhs.TotalScore(rhs.OpponentHand(),rhs.MyHandFromGuess());
                });
    }
    int Task2Score() override
    {
        return std::accumulate(iEncryptedStrategyGuide.begin(), iEncryptedStrategyGuide.end(), 0,
                [](int lhs, const Round& rhs)
                {
                    return lhs + rhs.TotalScore(rhs.OpponentHand(),rhs.MyHardFromStrategy());
                });
    }
private:
    std::vector<Round> iEncryptedStrategyGuide;
};

class ReportBuilder
{
public:
    ReportBuilder(IScoreCalculator *aCalculator)
    {
        std::cout << "--- Day 2: Rock Paper Scissors ---" << std::endl;
        std::cout << "Total score (based on guess):" << aCalculator->Task1Score() << std::endl;
        std::cout << "Total score (based on strategy):" << aCalculator->Task2Score() << std::endl;
    }
};

int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    Tournament tournament(argv[1]);
    ReportBuilder report(&tournament);

    return 0;
}
