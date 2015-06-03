job_count = 0

#pdfsets = [("CT10.LHgrid", 53),
#           ("cteq6ll.LHpdf", 44),
#           ("MSTW2008nlo68cl.LHgrid", 41),
#           ("MSTW2008lo68cl.LHgrid", 41),
#           ("NNPDF21_as_0118_100.LHgrid", 100),
#           ("NNPDF21_lo_as_0130_100.LHgrid", 100)]


from mh_detail import *
from prospino_inp import *
from bsub_inp import *

for m_H in m_Hs["14TeV"]:

#	span = 2.0/m_H[1] - 1.0/(m_H[1]*2.0)
#	step = span/100.0
#	scale = 1.0/(m_H[1]*2.0)	

	for i in range(10, 91):
		job_count += 1

		scale = (1.0/m_H[1])*i/30.0

		f = file("inputs_scale_var/input%d" % job_count, "w")

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
			"inverse_scafac" : 1.0/scale,  #inverse scale as taken from table times 30.0/i
			"scavar": 30.0/float(i),       #1 for central scale;
			                               #scavar 0.5 means inverse_scafac is 0.5*central
			"cH_mass" : m_H[0],
			"b_mass" : 4.75,
			"tanbeta" : 30.0
		}

		f.write(prospino_input % desc)
		f.close()

make_bsub_files("scale_var", "0:20", "2048", job_count)

print "Created %d input-files." % job_count

#scales:

#central:
#inverse_scafac = m_H[1]
#scavar         = 1

#central * 2
#inverse_scafac = m_H[1] / 2.0
#scavar         = 1 / 2.0

#central * 0.5
#inverse_scafac = m_H[1] * 2.0
#scavar         = 1 * 2.0

##################################################

#qf=scafac  * (m1+m2)
#qr=scafac2 * (m1+m2) / 2.

#qf= (m1+m2)        / inv_scafac
#qr= (m1+m2) / 2.   / scavar

#scafac = 1.0D0/inv_scafac
#scafac2 = 1.0D0/scavar

##################################################

#7 scales
#qf  , qr              inverse_scafac = m_H[1]            scavar         = 1
#qf*2, qr*2            inverse_scafac = m_H[1]/2.0        scavar         = 1/2.0
#qf/2, qr/2            inverse_scafac = m_H[1]*2.0        scavar         = 1*2.0
#qf  , qr*2            inverse_scafac = m_H[1]            scavar         = 1/2.0
#qf*2, qr              inverse_scafac = m_H[1]/2.0        scavar         = 1    
#qf/2, qr              inverse_scafac = m_H[1]*2.0        scavar         = 1    
#qf  , qr/2            inverse_scafac = m_H[1]            scavar         = 1*2.0
