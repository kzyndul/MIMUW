#include <iostream>
#include <regex>
#include <set>
#include <map>
#include <unordered_map>

using namespace std;


bool split(string const &line, vector<uint32_t> &result) {
    uint32_t n = 0;
    for(size_t i = 0; i < line.length(); i++) {
        if (isdigit(line[i])) {
            n *= 10;
            n += line[i] - '0';
        } else {
            if (n != 0) {
                result.push_back(n);
            }
            n = 0;
        }
    }

    if (n != 0) {
       result.push_back(n);
    }

    sort(result.begin(), result.end());

    return adjacent_find(result.begin(), result.end()) == result.end();
}

void chooseBest(unordered_map<uint32_t, uint32_t> &chooseBy, vector<pair<uint32_t, uint32_t>> &record) {
    for(auto& [id, votes] : chooseBy) // wybór 7 najlepszych utworów
        if (votes > record[0].second || (votes == record[0].second && id < record[0].first)) {
            record[0] = make_pair(id, votes);
            sort(record.begin(), record.end(), [](auto &left, auto &right) {
                return left.second < right.second || (left.second == right.second && left.first > right.first);
            });
        }
}

void pruint32_t(vector<pair<uint32_t, uint32_t>> &record, vector<uint32_t> &last){
    int place = 0;
    for (pair<uint32_t, uint32_t> p : record) // wypisanie podsumowania
        if (p.first && p.first != UINT32_MAX) {
            int i;
            for (i = 0; i < 7; i++)
                if (last[i] == p.first) {
                    cout << p.first << " " << i - place << endl;
                    break;
                }
            if (i == 7)
                cout << p.first << " -" << endl;
            place++;
        }

    place = 0;

}

void newMaxProcess(vector<uint32_t> &newMax, uint32_t &max, 
            unordered_map<uint32_t, uint32_t> &currVotes, vector<uint32_t> &lastRecord,
            unordered_map<uint32_t, uint32_t> &allPouint32_ts, vector<uint32_t> &droppedSongs) {
    max = newMax[0];
    vector<pair<uint32_t, uint32_t>> record(7, make_pair(UINT32_MAX, 0));

    chooseBest(currVotes, record);

    uint32_t place = 1;
    for (pair<uint32_t, uint32_t> p : record) // zapisywanie punktów
        if (p.first && p.first != UINT32_MAX)
            if (allPouint32_ts.find(p.first) != allPouint32_ts.end())
                (allPouint32_ts.find(p.first))->second = (allPouint32_ts.find(p.first))->second + place++;
            else
                allPouint32_ts.insert(make_pair(p.first, place++));
        else
            place++;

    for (uint32_t i : lastRecord) // wyrzucanie utworów
        if (find_if(record.begin(), record.end(),
                         [&i](const std::pair<uint32_t, uint32_t>& p){ return p.first == i;} ) == record.end())
            droppedSongs.push_back(i);

    sort(record.begin(), record.end(), [](auto &left, auto &right) { // odwrócenie kolejności
        return left.second > right.second || (left.second == right.second && left.first < right.first);});

    pruint32_t(record, lastRecord);
    place = 0;
    
    for (pair<uint32_t, uint32_t> p : record) // zapisanie notowania
        lastRecord[place++] = p.first;

    currVotes.clear();
}

void topProcess(unordered_map<uint32_t, uint32_t> &allPouint32_ts, vector<uint32_t> &lastTop) {
    vector<pair<uint32_t, uint32_t>> record(7, make_pair(UINT32_MAX, 0));

    chooseBest(allPouint32_ts, record);

    sort(record.begin(), record.end(), [](auto &left, auto &right) { // odwrócenie kolejności
        return left.second > right.second || (left.second == right.second && left.first < right.first);});


    pruint32_t(record, lastTop);

    uint32_t place = 0;
    for (pair<uint32_t, uint32_t> p : record) // zapisanie podsumowania
        lastTop[place++] = p.first;
}

bool voteProcess(vector<uint32_t> &newVote, unordered_map<uint32_t, uint32_t> &currVotes, 
                vector<uint32_t> &droppedSongs, uint32_t max) {
    for (uint32_t i : newVote) // sprawdzenie poprawności
        if (find(droppedSongs.begin(), droppedSongs.end(), i) != droppedSongs.end() || i > max)
            return false;

    for (uint32_t i : newVote) // przetwarzanie głosów
            if (currVotes.find(i) != currVotes.end())
                ((currVotes.find(i))->second)++;
            else
                currVotes.insert(make_pair(i, 1));

    return true;
}

void lineIssue(uint32_t lineNum, string line) {
    cerr << "Error in line " << lineNum << ": " << line << endl;
}

int main() {
    string line;
    regex numberPattern = regex("^\\s*[1-9]\\d{0,7}(\\s+[1-9]\\d{0,7})*\\s*$");
    regex newMaxPattern = regex("^\\s*NEW\\s+[1-9]\\d{0,7}\\s*$");
    regex topPattern = regex("^\\s*TOP\\s*$");
    regex emptyLinePattern = regex("^\\s*$");

    uint32_t max = 0;
    unordered_map<uint32_t, uint32_t> currVotes;
    unordered_map<uint32_t, uint32_t> allPouint32_ts;
    vector<uint32_t> lastRecord(7, 0);
    vector<uint32_t> lastTop(7, 0);
    vector<uint32_t> droppedSongs;

    uint32_t lineNum = 1;
    while (getline(cin, line)) {
        if (regex_match(line, numberPattern)) {
            vector<uint32_t> splitLine;
            if (!split(line, splitLine))
                lineIssue(lineNum, line);
            else if (!voteProcess(splitLine, currVotes, droppedSongs, max))
                lineIssue(lineNum, line);
        } else if (regex_match(line, newMaxPattern)) {
            vector<uint32_t> newMax;
            split(line, newMax);;
            if (newMax[0] < max)
                lineIssue(lineNum, line);
            else
                newMaxProcess(newMax, max, currVotes, lastRecord, allPouint32_ts, droppedSongs);
        } else if (regex_match(line, topPattern)) {
            topProcess(allPouint32_ts, lastTop);
        } else if (!regex_match(line, emptyLinePattern)){
            lineIssue(lineNum, line);
        }
        lineNum++;
    }
}
