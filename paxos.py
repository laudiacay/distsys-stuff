import sys
from enum import Enum
from itertools import groupby

# enum class for state of proposer
class PState(Enum):
    IDLE = 0
    SENTPREP = 1
    SENTACC = 2
    CONSENSUS = 3

# proposer class/object
class Proposer:

    # class variable, list of proposers
    proposers = []
    
    # initialize
    def __init__(self, num):
        self.num = num     # proposer number 
        self.failed = False     # failed? 
        self.initprop = None     # initial proposal (for final printout)
        self.topprop = None    # top proposal, gotten from accepters
        self.currentN = 0     # current proposal number
        self.promcount = 0    # count promises received
        self.acccount = 0     # "" but with accepts
        self.rejcount = 0     # "" but with rejections
        # keep track of what messages should be accepted by pnum, state
        # after consensus, the second number is the chosen value
        self.state = (PState.IDLE, None)

    # print for output
    def name(self):
        return "P%d" % self.num
    
    # initialize all proposers
    @classmethod
    def buildproposers(cls, num):
        cls.proposers = [Proposer(i+1) for i in range(num)]

    # print final state for last lines of output
    def finalstate(self):
        if self.state[0] == PState.CONSENSUS:
            return "%s has reached consensus (proposed %d, accepted %d)" % (self.name(), self.initprop, self.state[1])
        else:
            return "%s did not reach consensus" % self.name()

    # what to do on getting a promise message
    def recpromise(self, m, t):
        global numaccepters
        if m.p:
            on, ov = m.p
            print("%03d: A%d -> P%d  PROMISE n=%d (Prior: n=%d, v=%d)" % (t,
                    m.sndr, m.rcvr, m.n, on, ov))
        else: 
            print("%03d: A%d -> P%d  PROMISE n=%d (Prior: None)" % (t, m.sndr,
                m.rcvr, m.n))
        if self.state == (PState.SENTPREP, m.n):
            if m.p and (self.topprop == None or self.topprop[0] < m.p[0]):
                self.topprop = m.p
            self.promcount += 1
            v = self.initprop if self.topprop == None else self.topprop[1]
            if self.promcount * 2 > numaccepters:
                self.state = (PState.SENTACC, self.currentN)
                return [NetMessage(self.num, an+1, MType.ACCEPT, v=v,
                    n=self.currentN) for an in range(numaccepters)]
        return []
    
    # what to do when getting an accepted message
    def recaccepted(self, m, t):
        global numaccepters
        print("%03d: A%d -> P%d  ACCEPTED n=%d v=%d" % (t, m.sndr, m.rcvr, m.n,
            m.v))
        if self.state == (PState.SENTACC, m.n):
            self.acccount += 1
            if self.acccount * 2 > numaccepters:
                self.state = (PState.CONSENSUS, m.v)
        return []

    # what to do on getting a rejected message
    def recrejected(self, m, t):
        global numaccepters, pnum
        print("%03d: A%d -> P%d  REJECTED n=%d" % (t, m.sndr, m.rcvr, m.n))
        if self.state == (PState.SENTACC, m.n):
            self.rejcount += 1
            if self.rejcount * 2 > numaccepters:
                self.currentN = pnum
                self.state = (PState.SENTPREP, self.currentN)
                self.promcount = 0
                self.acccount = 0
                self.rejcount = 0
                pnum += 1
                return [NetMessage(self.num, an+1, MType.PREPARE,
                    n=self.currentN) for an in range(numaccepters)]
        return []

# accepted object class, mostly same as above
class Accepter:
    accepters = []
    
    def __init__(self, num):
        self.num = num
        self.failed = False
        self.prior = None # prior accepted value
        self.highN = 0 # highest N accepted
    
    def name(self):
        return "A%d" % self.num
    
    @classmethod
    def buildaccepters(cls, num):
        cls.accepters = [Accepter(i+1) for i in range(num)]
    
    # to do on receiving prepare
    def recprepare(self, m, t):
        print("%03d: P%d -> A%d  PREPARE n=%d" % (t, m.sndr, m.rcvr, m.n))
        if self.highN < m.n:
            self.highN = m.n
            return [NetMessage(self.num, m.sndr, MType.PROMISE, n=m.n,
                    p=self.prior)]
        return []
    
    # to do on receiving accept
    def recaccept(self, m, t):
        print("%03d: P%d -> A%d  ACCEPT n=%d v=%d" % (t, m.sndr, m.rcvr, m.n,
            m.v))
        if self.highN <= m.n:
            # accept
            self.prior = (m.n, m.v)
            return [NetMessage(self.num, m.sndr, MType.ACCEPTED, v=m.v, n=m.n)]
        else:
            #reject
            return [NetMessage(self.num, m.sndr, MType.REJECTED, n=m.n)]

# enums for message types
class MType(Enum):
    PREPARE = 0
    PROMISE = 1
    ACCEPT = 2
    ACCEPTED = 3
    REJECTED = 4

# the actual contents of the messages
class NetMessage:
    def __init__(self, sender, receiver, mtype, n=None, v=None, p=None):
        self.sndr = sender
        self.rcvr = receiver
        self.mtype = mtype
        self.n = n
        self.v = v
        self.p = p

    # check if deliverable
    def deliverable(self):
        if self.mtype in [MType.PREPARE, MType.ACCEPT]:
            senderup = not Proposer.proposers[self.sndr - 1].failed
            recverup = not Accepter.accepters[self.rcvr - 1].failed
        else:
            senderup = not Accepter.accepters[self.sndr - 1].failed
            recverup = not Proposer.proposers[self.rcvr - 1].failed
        return senderup and recverup
    
    # actually deliver the message
    def deliver(self, t):
        if self.mtype == MType.PREPARE:
            toEnq = Accepter.accepters[self.rcvr - 1].recprepare(self, t)
        elif self.mtype == MType.PROMISE:
            toEnq = Proposer.proposers[self.rcvr - 1].recpromise(self, t)
        elif self.mtype == MType.ACCEPT:
            toEnq = Accepter.accepters[self.rcvr - 1].recaccept(self, t)
        elif self.mtype == MType.ACCEPTED:
            toEnq = Proposer.proposers[self.rcvr - 1].recaccepted(self, t)
        elif self.mtype == MType.REJECTED:
            toEnq = Proposer.proposers[self.rcvr - 1].recrejected(self, t)
        for msg in toEnq:
            NetQueue.append(msg)

# double-LL node for queue implementation
class NetQueueNode:
    def __init__(self, message):
        self.message = message
        self.next = None
        self.prev = None

# double-LL implementation of queue
class NetQueue:
    head = None
    tail = None
    length = 0
    
    # append to the end of the queue
    @classmethod
    def append(cls, netm):
        net = NetQueueNode(netm)
        if cls.tail == None:
            cls.head = net
            cls.tail = net
            cls.length += 1
        else:
            cls.tail.next = net
            net.prev = cls.tail
            cls.tail = net
            net.next = None
            cls.length += 1
    
    # remove first deliverable message from queue and return contents
    @classmethod
    def nextMessage(cls):
        cur = cls.head
        while cur != None and not cur.message.deliverable():
            cur = cur.next
        if cur:
            if cur.prev:
                cur.prev.next = cur.next
            if cur.next:
                cur.next.prev = cur.prev
            if cls.head == cur:
                cls.head = cur.next
            if cls.tail == cur:
                cls.tail = cur.prev
            cls.length -= 1
            return cur.message
        return None

# event object- stores fails/recoveries and proposals
class Event:
    events = {}

    def __init__(self, strs):
        self.ts = int(strs[0][0])
        failstrs = [s for s in strs if s[1] == "FAIL"]
        recstrs = [s for s in strs if s[1] == "RECOVER"]
        pstrs = [s for s in strs if s[1] == "PROPOSE"]
        self.pfails = [int(s[3]) for s in failstrs if s[2] == "PROPOSER"] 
        self.afails = [int(s[3]) for s in failstrs if s[2] == "ACCEPTOR"]
        self.precover = [int(s[3]) for s in recstrs if s[2] == "PROPOSER"]
        self.arecover = [int(s[3]) for s in recstrs if s[2] == "ACCEPTOR"]
        pstr = None if pstrs == [] else pstrs[0]
        self.proposal = (int(pstr[2]), int(pstr[3])) if pstr else None
    
    # group events together and construct events
    @classmethod
    def buildevents(cls, elist):
        grouped = sorted(elist, key=lambda x: x[0])
        for k, g in groupby(grouped, lambda x: x[0]):
            cls.events[int(k)] = Event(list(g))

# getting input, loading into variables
inputs = [line.strip() for line in sys.stdin]

inits = inputs[0].split()

events = [s.split() for s in inputs[1:-1]]

numproposers = int(inits[0])

numaccepters = int(inits[1])

tmax = int(inits[2])

# constructing objects and initializing things
time = 0

pnum = 1

Proposer.buildproposers(numproposers)

Accepter.buildaccepters(numaccepters)

Event.buildevents(events)

# implementation from kattis
for i in range(tmax + 1):
    if NetQueue.length == 0 and len(Event.events) == 0:
        break
    try:
        e = Event.events[i]
    except:
        e = None
    if e:
        del Event.events[i]
        # do any failures/recoveries
        for c in e.pfails:
            Proposer.proposers[c - 1].failed = True
            print("%03d: ** P%d FAILS **" % (i, c))
        for c in e.afails:
            Accepter.accepters[c - 1].failed = True
            print("%03d: ** A%d FAILS **" % (i, c))
        for c in e.precover:
            Proposer.proposers[c - 1].failed = False
            print("%03d: ** P%d RECOVERS **" % (i, c))
        for c in e.arecover:
            Accepter.accepters[c - 1].failed = False
            print("%03d: ** A%d RECOVERS **" % (i, c))
        # process proposal event
        if e.proposal is not None:
            pc, pv = e.proposal
            Proposer.proposers[pc - 1].initprop = pv
            Proposer.proposers[pc - 1].currentN = pnum
            Proposer.proposers[pc - 1].state = (PState.SENTPREP, pnum)
            print("%03d:    -> P%d  PROPOSE v=%d" % (i, pc, pv))
            for a in range(numaccepters):
                NetQueue.append(NetMessage(pc, a + 1, MType.PREPARE,
                    n=pnum))
            pnum += 1
            continue
    # get the next message if we didn't propose anything
    m = NetQueue.nextMessage()
    if m:   
        m.deliver(i)
    # if we did nothing at all, just print the timestamp
    if not m and not e: 
        print("%03d: " % i)

print()

# print final state
for prop in Proposer.proposers:
    print(prop.finalstate())
