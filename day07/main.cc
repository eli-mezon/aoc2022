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


class IItem
{
public:
    virtual ~IItem() = default;
    virtual int Size() = 0;
};

class File
    : public IItem
{
public:
    File(const std::string& aLine)
        : iLine{aLine}
    {
    }
public: // IItem
    int Size() override
    {
        return std::stoi(iLine.substr(0, iLine.find(" ")));
    }
private:
    std::string iLine;
};

struct Directory
    : public IItem
{
public:
    Directory(
        Directory *aParent,
        const std::string& aLine)

        : iParent(aParent)
        , iLine(aLine)
    { 
    }
    Directory(const std::string& aLine)
        : Directory(this, aLine)
    {
    }

    void AddFile(const std::string& aFile)
    {
        std::cout << "add file[" << aFile << "]" << std::endl;
        iFiles.push_back(std::make_unique<File>(aFile));
    }

    void AddDir(const std::string& aDirName)
    {
        std::cout << "add dirr[" << aDirName << "]" << std::endl;
        iDirectories.push_back(std::make_unique<Directory>(this, aDirName));
    }

    Directory *FindDir(const std::string& aDirName)
    {
        std::cout << "find dirr[" << aDirName << "]" << std::endl;

        auto dir = std::find_if(iDirectories.begin(), iDirectories.end(),
            [aDirName](const std::shared_ptr<Directory> aDirectory)
            {
                return aDirectory->iLine.find(aDirName) != std::string::npos;
            });

        return (dir != iDirectories.end())
            ? (&(*dir))->get()
            : nullptr;
    }

public:
    int Size() override
    {
        int size = std::accumulate(iFiles.begin(), iFiles.end(), 0,
            [](int lhs, const std::unique_ptr<File>& rhs)
            {
                return lhs + rhs->Size();
            })
            + std::accumulate(iDirectories.begin(), iDirectories.end(), 0,
            [](int lhs, const std::shared_ptr<Directory> rhs)
            {
                return lhs + rhs->Size();
            });
//        std::cout << "TotalSizeOfAtMost[" << iLine << "] = " << size << std::endl;

        return  size;
    }
    void TotalSizeOfAtMost(int& aAcc, int aSize)
    {
        for_each(iDirectories.begin(), iDirectories.end(), 
            [&aAcc, aSize](const std::shared_ptr<Directory> rhs) 
            {
                int size = rhs->Size();
                aAcc += (size < aSize) ? size : 0;
                rhs->TotalSizeOfAtMost(aAcc, aSize);
            });
    }
    void SizeToDelete(int& aValue, int aFreeNow)
    {
        for_each(iDirectories.begin(), iDirectories.end(), 
            [&aValue, aFreeNow](const std::shared_ptr<Directory> rhs) 
            {
                int size = rhs->Size();
                if (aFreeNow + size >= 30000000)
                {
                    aValue = (size < aValue) ? size : aValue;
                }
                rhs->SizeToDelete(aValue, aFreeNow);
            });
    }

public:
    Directory *iParent;
    std::string iLine;
    std::list<std::unique_ptr<File>> iFiles;
    std::list<std::shared_ptr<Directory>> iDirectories;

};

class FileSystem
    : public IScoreCalculator
{
public:
    FileSystem(const std::string& aFileName)
        : iRoot(Directory("/"))
        , iCurrent(&iRoot)
    {
        std::ifstream list(aFileName);
        for (std::string line; std::getline(list, line); ) 
        {
//            std::cout << std::endl << "line[" << line << "]: ";
            if (line.find("$ ls") != std::string::npos)
            {
                continue;
            }
            else if (line.find("$ cd") != std::string::npos)
            {
                std::string dirName = line.substr(5);
                iCurrent = ChangeDir(dirName);
            }
            else if (line.find("dir") != std::string::npos)
            {
                std::string dirName = line.substr(4);
                MakeDir(dirName);
            }
            else
            {
                Touch(line);
            }
        }
    }

    void Touch(const std::string& aFile)
    {
        iCurrent->AddFile(aFile);
    }
    Directory* ChangeDir(const std::string& aDirName)
    {
        if (aDirName == "/") return &iRoot;
        if (aDirName == "..") return iCurrent->iParent;

        return iCurrent->FindDir(aDirName);
    }
    void MakeDir(const std::string& aDirName)
    {
        iCurrent->AddDir(aDirName);
    }
private: // IScoreCalculator
    std::string Header() override
    {
        return "--- Day 7: No Space Left On Device ---";
    }
    std::string  Task1Score() override
    {
        int acc = 0;
        iRoot.TotalSizeOfAtMost(acc, 100000);
        return std::to_string(acc);
    }
    std::string  Task2Score() override
    {
        int todelete = iRoot.Size();
        iRoot.SizeToDelete(todelete, 70000000 - iRoot.Size());
        return std::to_string(todelete);
    }
private:
    Directory iRoot;
    Directory* iCurrent;
};

//////////////////////////////////////////////////////////////////////////////


int main(int argc, char ** argv)
{
    if (argc < 2) return -1;

    FileSystem fileSystem(argv[1]);
    ReportBuilder report(&fileSystem);

    return 0;
}
