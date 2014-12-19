// adapted for Visual C++ from: https://github.com/rdbisch/Santa2014


#ifndef _NAIVE_H
#define _NAIVE_H

#include "Elf.h"
#include "Toy.h"
#include "Hours.h"


class NaiveSolution {
    priority_queue<Elf> elves;

public:
    NaiveSolution(int n, string toyFile) {
        for (int i = 0; i < n; ++i) elves.push(Elf(i+1));

        ifstream inToys(toyFile);
        string line;
       
        cout << "ToyId,ElfId,StartTime,Duration" << endl;
        getline(inToys, line);     //skip header

        while (std::getline(inToys, line)) {
            Toy T(line);

            Elf E = elves.top();
            elves.pop();

            int time = max(T.arrivalTime, E.next_available_time);
            int actual_duration = E.update_elf(time, T.duration);
            char timestr[32];
            printf("%d,%d,%s,%d\n", T.id, E.id, timeToString(time, timestr), actual_duration);
            //cout << T.id << "," << E.id << "," << timeToString(time,timestr) << "," << actual_duration << endl;
            elves.push(E);
        }
    }
};

#endif
