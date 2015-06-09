import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime

# datetime.strptime

def mknum(s):
    if s.endswith("g"):
        s_m = float(s[:-1]) * 1000
    elif s.endswith("m"):
        s_m = float(s[:-1])
    elif s.endswith("k"):
        s_m = float(s[:-1]) / 1000
    else:
        s_m = float(s)
    
    return s_m

fh = open("top-stats.txt", "r")

x = []
y_resv = []
y_used = []
y_cpu  = []
y_perc = []

for line in fh:
    nline_str = line.replace("  ", " ").replace("  ", " ").replace("  ", " ").replace("  ", " ")
    nline = nline_str.split(" ")
    # ['\x1b[m\x0f\x1b[m\x0f20778', 'ashuang1', '20', '0', '30.8g', '30g', '2768', 'R', '100', '16.3', '5:01.29', 'test_netcdf_lay', '', '', '', '', '', '\x1b[m\x0f\x1b[39;49m\n']
    # Reserved: nline[4]
    # Actually used: nline[5]
    # Mem percent: nline[9]
    # Time: nline[10]
    memreserved = nline[4]
    memused = nline[5]
    cpupercent = nline[8]
    mempercent = nline[9]
    
    # Convert memory
    memreserved = mknum(memreserved)
    memused = mknum(memused)
    cpupercent = mknum(cpupercent)
    mempercent = mknum(mempercent)
        
    ts = nline[10] #5:01.29 - min:sec
    ts_dt = datetime.strptime(ts, "%M:%S.%f")
    
    x.append(ts_dt)
    y_resv.append(memreserved)
    y_used.append(memused)
    y_perc.append(mempercent)
    y_cpu.append(cpupercent)

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
ax1.plot(x, y_resv, label="Reserved memory (malloc)")
ax1.plot(x, y_used, label="Used memory")

ax1.set_title('Memory Usage vs. Time')
ax1.legend()

fig2 = plt.figure()
ax2 = fig2.add_subplot(211)
ax2.plot(x, y_perc, label="Memory usage %")
ax2.legend()
ax2.set_title('Memory Usage % vs. Time')

ax3 = fig2.add_subplot(212)
ax3.plot(x, y_cpu, label="CPU usage %")
ax3.legend(loc='lower center')
ax3.set_title('CPU Usage % vs. Time')

#fig3 = plt.figure()
#ax3 = fig3.add_subplot(111)
#ax3.plot(x, y_cpu, label="CPU usage %")
#ax3.legend()

fig1.savefig("memusage.png")
fig2.savefig("percentusage.png")

plt.show()
