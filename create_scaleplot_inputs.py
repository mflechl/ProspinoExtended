job_count = 0

from mh_detail import *
from prospino_inp import *
from bsub_inp import *

for m_H in m_Hs["14TeV"]:

	if m_H[0] != 600 and m_H[0] != 200:
		continue

        if m_H[0] == 200:
		MUMIN=45
		MUMAX=745
		MUSTEP=10

        if m_H[0] == 600:
		MUMIN=95
		MUMAX=1545
		MUSTEP=20

	for imu in range(MUMIN,MUMAX+1,MUSTEP):
		job_count += 1

#                fscale = (1.0/m_H[1])*i/40.0
#                fscale=1/fscale

# 	 	 fscale = 2.0*40/i           #8-0.5
#                rscale = fscale/float(2)    #factor 2 is added by prospino

		fscale = (172.5+m_H[0])/imu    #about 8-0.5
                rscale = fscale/float(2)    #factor 2 is added by prospino

		f = file("inputs_scaleplot/input%d" % job_count, "w")

		desc = {
			"collider" : 1,
			"final_state" : "ht",
			"particle1" : 1,
			"particle2" : 1,
			"pdf_lo" : "cteq6ll.LHpdf",
			"member_lo" : 0,
			"use_pdfals_lo" : ".true.",
			"pdf_nlo" : "CT10.LHgrid\n",
			"member_nlo" : 0,
			"use_pdfals_nlo" : ".true.",
			"inverse_scafac" : fscale,
			"scavar": rscale,
			"cH_mass" : m_H[0],
			"b_mass" : 4.75,
			"tanbeta" : 30.0			
		}

		f.write(prospino_input % desc)
		f.close()

make_bsub_files("scaleplot", "0:20", "2048", job_count)

print "Created %d input-files." % job_count
