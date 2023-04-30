#include <stack>
#include <vector>
#include <string>
#include <array>
#include <unordered_set>
#include <algorithm>
#include <cassert>
#include <iostream>
#include "boost/format.hpp"

using namespace std;

enum Edge   { EPSILON = 256, END = -1 };
enum Op     { CONCAT  = 255 };

class State {
public:
    int                     c{EPSILON};
    State*                  o1{nullptr};
    State*                  o2{nullptr};

    State()=default;
    State(int c, State *o1=EndState(), State *o2=EndState()): c(c), o1(o1), o2(o2) {
    }


    static State* EndState() {
        static State end{-1, nullptr, nullptr};
        return &end;
    }

    bool fullMatch(const string& src) {
        unordered_set<State*> cur;
        unordered_set<State*> nxt;

        epsilonClosure(this, cur);
        for (char c: src) {
            for (auto s: cur) {
                if (s->c == c) {
                    if (s->o1) epsilonClosure(s->o1, nxt);
                }
            }
            
            cur.swap(nxt);
            nxt.clear();
        }
        return cur.count(EndState());
    }

    void print(std::ostream& os) const {
        stack<pair<const State*,int>> dfs;
        unordered_map<const State*, int> visited;
        int num = 0;

        dfs.push({this, num});
        visited.emplace(this, num);
        while (!dfs.empty()) {
            auto [s,n0] = dfs.top();
            dfs.pop();

            int n1, n2;
            if (s->o1) {
                auto it = visited.find(s->o1);
                if (it != visited.end()) {
                    n1 = it->second;
                }else {
                    n1 = ++num;
                    visited.emplace(s->o1, n1);
                    dfs.emplace(s->o1, n1);
                }
            }
            if (s->o2) {
                auto it = visited.find(s->o2);
                if (it != visited.end()) {
                    n2 = it->second;
                }else {
                    n2 = ++num;
                    visited.emplace(s->o2, n2);
                    dfs.emplace(s->o2, n2);
                }
            }
            os << boost::format("S%1%(%2%) --(%3%)--> S%4%(%5%),S%6%(%7%)") 
                % n0 % s % s->c % n1 % s->o1 % n2 % s->o2 << endl;
        }
    }

private:
    void epsilonClosure(State *s, unordered_set<State*>& st) {
        stack<State*> dfs;;
        dfs.push(s);
        st.insert(s);
        while (!dfs.empty()) {
            auto s = dfs.top();
            dfs.pop();
            if (s->c == EPSILON) {
                if (s->o1 && st.insert(s->o1).second) dfs.push(s->o1);
                if (s->o2 && st.insert(s->o2).second) dfs.push(s->o2);
            }
        }
    }
};


class Frag {
public:
    Frag(State* s, vector<State*> ss): start(s), outs(std::move(ss)) {
    }

    Frag(char c) {
        start = new State{c};
        outs.push_back(start);
    }

    State* getStart() {
        return start;
    }

    Frag& operator|(const Frag& f) {
        start = new State{EPSILON, start, f.start};
        outs.insert(outs.end(), f.outs.begin(), f.outs.end());
        return *this;
    }

    Frag& operator+(const Frag& f) {
        for (auto out: outs) {
            out->o1 = f.start;
        }
        outs = f.outs;
        return *this;
    }

    Frag& zeroOrMore() {
        start = new State{EPSILON, State::EndState(), start};
        for (auto out: outs) {
            out->o1 = start;
        }
        outs.clear();
        outs.push_back(start);
        return *this;
    }

private:
    State*         start;
    vector<State*> outs;
};

static const array<int, 256> precedence = []() {
    array<int, 256> a;
    a['*']      = 3;
    a[CONCAT]   = 2;
    a['|']      = 1;
    return a;
}();

State* compile(const string& src) {
    stack<Frag>  acc;
    stack<int>   ops;

    auto calc = [&](int c) {
        switch (c) {
            case '|': {
                auto right = std::move(acc.top());
                acc.pop();
                auto left = std::move(acc.top());
                acc.pop();
                acc.push(left | right);
                break;
            }
            case CONCAT: {
                auto right = std::move(acc.top());
                acc.pop();
                auto left = std::move(acc.top());
                acc.pop();
                acc.push(left + right);
                break;
            }
            case '*': {
                auto frag = std::move(acc.top());
                acc.pop();
                acc.push(frag.zeroOrMore());
                break;
            }
            default:
                throw std::runtime_error("unsupported operator " + to_string(c));
        }
    };

    auto checkPushOp = [&](int c) {
        while (!ops.empty() && precedence[ops.top()] >= precedence[c]) {
            int op = ops.top();
            ops.pop();
            calc(op);
        }
        ops.push(c);
    };
    char prev = -1;
    for (char c: src) {
        switch (c) {
            case '|': 
            case '*': {
                checkPushOp(c);
                break;
            }
            case '(': {
                if (prev != -1 && prev != '|') {
                    checkPushOp(CONCAT);
                }
                ops.push(c);
                break;
            }
            case ')': {
                while (!ops.empty() && ops.top() != '(') {
                    calc(ops.top());
                    ops.pop();
                }
                ops.pop(); //pop '('
                break;
            }
            default: {
                if (prev != -1 && prev != '(' && prev != '|') { //insert virtual CONCAT operator
                    checkPushOp(CONCAT);
                }
                acc.push(Frag(c));
            }
        }
        prev = c;
    }

    //eval rest expression
    while (!ops.empty()) {
        calc(ops.top());
        ops.pop();
    }

    if (acc.size() != 1) {
        throw std::runtime_error("there must be some bugs in RPN convertion \
                or evaluation or regular expression is invalid");
    }
    return acc.top().getStart();
}


int main()  {
    assert(compile("1")->fullMatch("1"));
    assert(compile("12")->fullMatch("12"));
    assert(compile("(12)")->fullMatch("12"));
    assert(!compile("12")->fullMatch("21"));
    assert(compile("abcd")->fullMatch("abcd"));
    assert(!compile("abcd")->fullMatch("abcdef"));
    assert(compile("a|c")->fullMatch("a"));
    assert(compile("a|c")->fullMatch("c"));
    assert(compile("ab|cd")->fullMatch("ab"));
    assert(compile("ab|cd")->fullMatch("cd"));
    assert(!compile("ab|cd")->fullMatch("acd"));
    assert(compile("(ab)|(cd)")->fullMatch("cd"));
    assert(!compile("(ab|12)|(34|cd)")->fullMatch("ab34"));
    assert(compile("a(1|2|3|4)b")->fullMatch("a1b"));
    assert(compile("a(1|2|3|4)b")->fullMatch("a4b"));
    assert(!compile("a(1|2|3|4)b")->fullMatch("a24b"));
    assert(compile("a*")->fullMatch(""));
    assert(compile("a*")->fullMatch("a"));
    assert(compile("a*")->fullMatch("aa"));
    assert(compile("a*")->fullMatch("aaaa"));
    assert(compile("(a*)*")->fullMatch("aaaa"));
    assert(!compile("ab*")->fullMatch("aaaa"));
    assert(compile("ab*")->fullMatch("a"));
    assert(compile("ab*")->fullMatch("ab"));
    assert(compile("ab*")->fullMatch("abbbbb"));
    assert(compile("a|b*")->fullMatch("a"));
    assert(compile("a|b*")->fullMatch("bbbbb"));
    assert(!compile("a|b*")->fullMatch("aaaa"));
    assert(!compile("a|b*")->fullMatch("abab"));
    assert(compile("(ab)*")->fullMatch(""));
    assert(compile("(ab)*")->fullMatch("ababab"));
    assert(!compile("(ab)*")->fullMatch("ababa"));
    assert(!compile("(ab)*")->fullMatch("abababbb"));
    assert(compile("a*b*c*")->fullMatch(""));
    assert(compile("a*b*c*")->fullMatch("abc"));
    assert(compile("a*b*c*")->fullMatch("bbc"));
    assert(compile("a*b*c*")->fullMatch("bbbb"));
    assert(compile("a*b*c*")->fullMatch("c"));
    assert(!compile("a*b*c*")->fullMatch("bcaa"));
    assert(compile("a*b*c*")->fullMatch("aaaccccccc"));
    assert(compile("1(a|b)*(a|b)0")->fullMatch("1b0"));
    assert(compile("1(a|b)*(a|b)0")->fullMatch("1abbba0"));
    assert(compile("1*(abc|def)*2*")->fullMatch("2"));
    assert(!compile("1*(abc|def)*2*")->fullMatch("1111abcef"));
}
