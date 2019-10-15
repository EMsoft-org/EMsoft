

#include <cstdio>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "TestFileLocations.h"
// clang-format off
static const std::vector<std::string> files = {"dp-Ni6.h5",
                                              "Ni-master-20kV.h5",
                                              "Ni6-matrix.tiff",
                                              "Ni6-reference.tiff",
                                              "NiScan6_ADP.tiff",
                                              "PatternTest.h5",
                                              "result-fit-Ni6.ctf",
                                              "result-Ni6.ang",
                                              "result-Ni6.ctf"};
// clang-format on
static const std::string k_TutorialDir("DItutorial/Ni");

/**
 * @brief exists Checks if a file exists
 * @param name
 * @return
 */
inline bool exists(const std::string& name)
{
  if(FILE* file = fopen(name.c_str(), "r"))
  {
    fclose(file);
    return true;
  }
  return false;
}

/**
 * @brief main This program will attempt to delete previous output files from the Dictionary Indexing workflow ctest
 * @param argc
 * @param argv
 * @return
 */
int main(int argc, char* argv[])
{

  for(const auto& file : files)
  {
    std::stringstream ss;
    ss << EMsoft::Test::EMsoftBinaryDir << "/" << k_TutorialDir << "/" << file;

    if(exists(ss.str()))
    {
      std::cout << "Deleting " << ss.str() << std::endl;
      std::string filepath = ss.str();
#ifdef _WIN32
      std::replace(filepath.begin(), filepath.end(), '/', '\\');
#endif
      std::remove(filepath.c_str());
    }
  }

  return 0;
}
