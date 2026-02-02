        implicit real*8(a-h,o-z) 
        parameter (nsolid=26,ndiss=29,ncomp=55,nreac=60,nx=241)
        common/kinetics/sw01,sw02,sw03,sw04,sw05,sw06,sw07,sw08,sw09,sw1
     +0,sw11,sw12,sw13,sw14,sw15,sw16,sw17,sw18,sw19,sw20,sw21,sw22,sw23
     +,sw24,sw25,sw26,sw27,sw28,sw29,sw30,sw31,sw32,xbiot,inita,nu,kmo2,
     +kmno3,kmmno2,kmfeoh3,kmso4,knit,kmnox,kfemno2,kfeo2,kh2so2,kh2smno
     +2,kh2sfeoh3,kaom,kaomo2,kfeso2,kmnco3precip,KsMnCO3,kfesprecip,kfe
     +sdiss,KsFeS,kfeco3precip,KsFeCO3,kcaldiss,kcalppt,karadis,karappt,
     +kmgcdis,kmgcppt,kpyr,kfess0,kapa,po4_eq,kdis,h2sstar,ch4eq,kgas,kd
     +i,ksnh4,kspo4,ksfp,ksfe,kf1,kb1,kf2,kb2,kf3,kb3,kf4,kb4,kf5,kb5,kf
     +6,kb6,kf7,kb7,kf8,kb8,kspcal,kspara,kspmgc,keq1,keq2,keq3,keq4,kfe
     +mno2pr,kh2smno2pr,kmnage,kh2sfeoh3mr,kh2sfeoh3pr,kfeage,keq5,kBSid
     +is,ad,bd,BSisat,ksfsi,kf9,kb9,eq1_al,eq2_al,eq3_al,eq4_al,kf10,kb1
     +0,kf11,kb11,kf12,kb12,kf13,kb13,kspglass,kglass,kspplagio,kplagio,
     +kspolive,kolive,ksppyrox,kpyrox,kspillite,killite,kspsmect,ksmect,
     +kspkaoli,kkaoli
        real*8 sw01,sw02,sw03,sw04,sw05,sw06,sw07,sw08,sw09,sw10,sw11,sw
     +12,sw13,sw14,sw15,sw16,sw17,sw18,sw19,sw20,sw21,sw22,sw23,sw24,sw2
     +5,sw26,sw27,sw28,sw29,sw30,sw31,sw32,xbiot,inita,nu,kmo2,kmno3,kmm
     +no2,kmfeoh3,kmso4,knit,kmnox,kfemno2,kfeo2,kh2so2,kh2smno2,kh2sfeo
     +h3,kaom,kaomo2,kfeso2,kmnco3precip,KsMnCO3,kfesprecip,kfesdiss,KsF
     +eS,kfeco3precip,KsFeCO3,kcaldiss,kcalppt,karadis,karappt,kmgcdis,k
     +mgcppt,kpyr,kfess0,kapa,po4_eq,kdis,h2sstar,ch4eq,kgas,kdi,ksnh4,k
     +spo4,ksfp,ksfe,kf1,kb1,kf2,kb2,kf3,kb3,kf4,kb4,kf5,kb5,kf6,kb6,kf7
     +,kb7,kf8,kb8,kspcal,kspara,kspmgc,keq1,keq2,keq3,keq4,kfemno2pr,kh
     +2smno2pr,kmnage,kh2sfeoh3mr,kh2sfeoh3pr,kfeage,keq5,kBSidis,ad,bd,
     +BSisat,ksfsi,kf9,kb9,eq1_al,eq2_al,eq3_al,eq4_al,kf10,kb10,kf11,kb
     +11,kf12,kb12,kf13,kb13,kspglass,kglass,kspplagio,kplagio,kspolive,
     +kolive,ksppyrox,kpyrox,kspillite,killite,kspsmect,ksmect,kspkaoli,
     +kkaoli
        common/physics/al,q0,w0,Db0,por0,area0,t_celsius,salin,delt,dept
     +hmax,endt
        real*8 al,q0,w0,Db0,por0,area0,t_celsius,salin,delt,depthmax,end
     +t
        common/physics2/iq,iw,iDb,ipor,igrid,iarea,ic
        integer iq,iw,iDb,ipor,igrid,iarea,ic
