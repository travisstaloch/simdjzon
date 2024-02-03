#include "simdjson.h"
using namespace simdjson;
int main(int argc, char** argv) {
    if(argc != 2) {
        std::cout << "USAGE: ./simdjson <file.json>" << std::endl;
        exit(1);
    }
    dom::parser parser; 
    try
    {
        const dom::element doc = parser.load(argv[1]);
        int i = 0;
        for(auto status: doc["statuses"]) {
            // std::cout << status["id"] << '\n';
            i += 1;
        }
        if(i != 100) {
            std::cout << "error. expected i=100. found i=" << i << '\n';
            return 1;
        }
    }
    catch(const std::exception& e)
    {
        std::cerr << e.what() << '\n';
        return 1;
    }
    return 0;
}
