job_count = 0

from mh_detail import *
from prospino_inp import *
from bsub_inp import *

tbs = [ 2.0 , 10.0 , 50.0 ]
#tbs = ( 2.0 , 10.0 , 30.0 , 50.0 )

for m_H in m_Hs["14TeV"]:
#for m_H in m_Hs["8TeV"]:

    if m_H[0] != 600 and m_H[0] != 200:
                continue

    for tb in tbs:

        scales = [ ( m_H[1]     , 1.0     , 1.0     ),	
		   ( m_H[1]/2.0 , 1.0/2.0 , 1.0/2.0 ),
		   ( m_H[1]*2.0 , 1.0*2.0 , 1.0*2.0 ),
		   ( m_H[1]     , 1.0/2.0 , 1.0/2.0 ),
		   ( m_H[1]/2.0 , 1.0     , 1.0     ),
		   ( m_H[1]*2.0 , 1.0     , 1.0     ),
		   ( m_H[1]     , 1.0*2.0 , 1.0*2.0 ),
		   ( m_H[1]     , 1.0     , 1.0/2.0 ),
		   ( m_H[1]     , 1.0     , 1.0*2.0 ),
		   ( m_H[1]/2.0 , 1.0/2.0 , 1.0     ),
		   ( m_H[1]*2.0 , 1.0*2.0 , 1.0     ),
		   ( m_H[1]     , 1.0/2.0 , 1.0     ),
		   ( m_H[1]/2.0 , 1.0     , 1.0/2.0 ),
		   ( m_H[1]*2.0 , 1.0     , 1.0*2.0 ),
		   ( m_H[1]     , 1.0*2.0 , 1.0     )]

        for scale in scales:
		job_count += 1
			
		f = file("inputs_scale_var_vs_tb/input%d" % job_count, "w")

		desc = {
			"collider" : 1,
#			"collider" : 3,
			"final_state" : "ht",
			"particle1" : 1,
			"particle2" : 1,
			"pdf_lo" : "cteq6ll.LHpdf",
			"member_lo" : 0,
			"use_pdfals_lo" : ".true.",
			"pdf_nlo" : "CT10.LHgrid\n",
			"member_nlo" : 0,
			"use_pdfals_nlo" : ".true.",
			"inverse_scafac" : scale[0],  #inverse scale as taken from table times 30.0/i
			"scavar": scale[1],       #1 for central scale; #scavar 0.5 means inverse_scafac is 0.5*central
			"scavar3": scale[2],       #1 for central scale; #scavar 0.5 means inverse_scafac is 0.5*central
			"cH_mass" : m_H[0],
			"b_mass" : 4.75,
			"tanbeta" : tb
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

