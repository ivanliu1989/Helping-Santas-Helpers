// adapted for Visual C++ from: https://github.com/rdbisch/Santa2014
#ifndef _ELF_H
#define _ELF_H


#include "Toy.h"

class Elf {
public:
    int id;
    double rating;
    int next_available_time;
public:
    // default constructor for containers
    Elf() : id(-1), rating(0.0), next_available_time(-1) { }

    Elf(int ID) : id(ID), rating(1.0), next_available_time(540) { }

    /** 
     * Update the elf's next_available_time and productivity ratings
     */
    int update_elf(int start_minute, int duration) {
        int real_duration = (int)ceil(duration / rating);
        int end_minute = start_minute + real_duration;

        int U, S; S=getSanctionedBreakdown(start_minute, real_duration,&U);

        next_available_time = applyRestingPeriod(end_minute, U);
        rating = max(0.25, min(4.0, rating * pow(1.02, S / 60.0) * pow(0.90, U / 60.0)));

        return real_duration;
    }

    int expectedDuration(const Toy& T) const {
        return (int)ceil(T.duration / rating);
    }

    string toString() const {
        stringstream out; char timestr[32];
        out << "(id: " << id << " rating: " << rating << " next: " << timeToString(next_available_time,timestr) << ")";
        return out.str();
    }

    bool operator<(const Elf& f) const {
        if ( next_available_time < f.next_available_time ) return false;
        else if ( next_available_time == f.next_available_time ) return id > f.id;
        else return true;
    }
};

#endif
