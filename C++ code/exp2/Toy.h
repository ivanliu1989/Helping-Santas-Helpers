// adapted for Visual C++ from: https://github.com/rdbisch/Santa2014

#ifndef _TOY_H
#define _TOY_H

#include "Hours.h"


/**
 * This object is just a handy
 * container for Toys and doesn't 
 * really do anything else.
 */
class Toy {
public:
    int id;
    int arrivalTime;
    int duration;

public:
    // default constructor for containers
    Toy() : id(-1), arrivalTime(-1), duration(-1) { }

    Toy(std::string line) {
        //ToyId, Arrival_time, Duration
        //1, 2014 1 1 0 1, 5

        int year, month, day, hour, minute;
        const char*p = line.c_str();
        sscanf(p, "%d, %d %d %d %d %d, %d", &id, &year, &month, &day, &hour, &minute, &duration);
        arrivalTime = stringToTime(year, month, day, hour, minute);
    }

    string toString() const {
        stringstream out; char timestr[32];
        out << "(toy " << id << " arrival " << timeToString(arrivalTime,timestr) << " duration " << duration << ")";
        return out.str();
    }

    bool operator<(const Toy& t) const {
        return id < t.id;
    }
};

#endif
