package io.teaql.data.web;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import cn.hutool.core.codec.Base64;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.net.URLEncodeUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;

public class BlobObject {
    public static final String TYPE_X3D = "application/vnd.hzn-3d-crossword";
    public static final String TYPE_3GP = "video/3gpp";
    public static final String TYPE_3G2 = "video/3gpp2";
    public static final String TYPE_MSEQ = "application/vnd.mseq";
    public static final String TYPE_PWN = "application/vnd.3m.post-it-notes";
    public static final String TYPE_PLB = "application/vnd.3gpp.pic-bw-large";
    public static final String TYPE_PSB = "application/vnd.3gpp.pic-bw-small";
    public static final String TYPE_PVB = "application/vnd.3gpp.pic-bw-var";
    public static final String TYPE_TCAP = "application/vnd.3gpp2.tcap";
    public static final String TYPE_7Z = "application/x-7z-compressed";
    public static final String TYPE_ABW = "application/x-abiword";
    public static final String TYPE_ACE = "application/x-ace-compressed";
    public static final String TYPE_ACC = "application/vnd.americandynamics.acc";
    public static final String TYPE_ACU = "application/vnd.acucobol";
    public static final String TYPE_ATC = "application/vnd.acucorp";
    public static final String TYPE_ADP = "audio/adpcm";
    public static final String TYPE_AAB = "application/x-authorware-bin";
    public static final String TYPE_AAM = "application/x-authorware-map";
    public static final String TYPE_AAS = "application/x-authorware-seg";
    public static final String TYPE_AIR =
            "application/vnd.adobe.air-application-installer-package+zip";
    public static final String TYPE_SWF = "application/x-shockwave-flash";
    public static final String TYPE_FXP = "application/vnd.adobe.fxp";
    public static final String TYPE_PDF = "application/pdf";
    public static final String TYPE_PPD = "application/vnd.cups-ppd";
    public static final String TYPE_DIR = "application/x-director";
    public static final String TYPE_XDP = "application/vnd.adobe.xdp+xml";
    public static final String TYPE_XFDF = "application/vnd.adobe.xfdf";
    public static final String TYPE_AAC = "audio/x-aac";
    public static final String TYPE_AHEAD = "application/vnd.ahead.space";
    public static final String TYPE_AZF = "application/vnd.airzip.filesecure.azf";
    public static final String TYPE_AZS = "application/vnd.airzip.filesecure.azs";
    public static final String TYPE_AZW = "application/vnd.amazon.ebook";
    public static final String TYPE_AMI = "application/vnd.amiga.ami";
    // public static final String TYPE_/A= "application/andrew-inset";
    public static final String TYPE_APK = "application/vnd.android.package-archive";
    public static final String TYPE_CII = "application/vnd.anser-web-certificate-issue-initiation";
    public static final String TYPE_FTI = "application/vnd.anser-web-funds-transfer-initiation";
    public static final String TYPE_ATX = "application/vnd.antix.game-component";
    public static final String TYPE_DMG = "application/x-apple-diskimage";
    public static final String TYPE_MPKG = "application/vnd.apple.installer+xml";
    public static final String TYPE_AW = "application/applixware";
    public static final String TYPE_LES = "application/vnd.hhe.lesson-player";
    public static final String TYPE_SWI = "application/vnd.aristanetworks.swi";
    public static final String TYPE_S = "text/x-asm";
    public static final String TYPE_ATOMCAT = "application/atomcat+xml";
    public static final String TYPE_ATOMSVC = "application/atomsvc+xml";
    // public static final String TYPE_ATOM, .XML= "application/atom+xml";
    public static final String TYPE_AC = "application/pkix-attr-cert";
    public static final String TYPE_AIF = "audio/x-aiff";
    public static final String TYPE_AVI = "video/x-msvideo";
    public static final String TYPE_AEP = "application/vnd.audiograph";
    public static final String TYPE_DXF = "image/vnd.dxf";
    public static final String TYPE_DWF = "model/vnd.dwf";
    public static final String TYPE_PAR = "text/plain-bas";
    public static final String TYPE_BCPIO = "application/x-bcpio";
    public static final String TYPE_BIN = "application/octet-stream";
    public static final String TYPE_BMP = "image/bmp";
    public static final String TYPE_TORRENT = "application/x-bittorrent";
    public static final String TYPE_COD = "application/vnd.rim.cod";
    public static final String TYPE_MPM = "application/vnd.blueice.multipass";
    public static final String TYPE_BMI = "application/vnd.bmi";
    public static final String TYPE_SH = "application/x-sh";
    public static final String TYPE_BTIF = "image/prs.btif";
    public static final String TYPE_REP = "application/vnd.businessobjects";
    public static final String TYPE_BZ = "application/x-bzip";
    public static final String TYPE_BZ2 = "application/x-bzip2";
    public static final String TYPE_CSH = "application/x-csh";
    public static final String TYPE_C = "text/x-c";
    public static final String TYPE_CDXML = "application/vnd.chemdraw+xml";
    public static final String TYPE_CSS = "text/css";
    public static final String TYPE_CDX = "chemical/x-cdx";
    public static final String TYPE_CML = "chemical/x-cml";
    public static final String TYPE_CSML = "chemical/x-csml";
    public static final String TYPE_CDBCMSG = "application/vnd.contact.cmsg";
    public static final String TYPE_CLA = "application/vnd.claymore";
    public static final String TYPE_C4G = "application/vnd.clonk.c4group";
    public static final String TYPE_SUB = "image/vnd.dvb.subtitle";
    public static final String TYPE_CDMIA = "application/cdmi-capability";
    public static final String TYPE_CDMIC = "application/cdmi-container";
    public static final String TYPE_CDMID = "application/cdmi-domain";
    public static final String TYPE_CDMIO = "application/cdmi-object";
    public static final String TYPE_CDMIQ = "application/cdmi-queue";
    public static final String TYPE_C11AMC = "application/vnd.cluetrust.cartomobile-config";
    public static final String TYPE_C11AMZ = "application/vnd.cluetrust.cartomobile-config-pkg";
    public static final String TYPE_RAS = "image/x-cmu-raster";
    public static final String TYPE_DAE = "model/vnd.collada+xml";
    public static final String TYPE_CSV = "text/csv";
    public static final String TYPE_CPT = "application/mac-compactpro";
    public static final String TYPE_WMLC = "application/vnd.wap.wmlc";
    public static final String TYPE_CGM = "image/cgm";
    public static final String TYPE_ICE = "x-conference/x-cooltalk";
    public static final String TYPE_CMX = "image/x-cmx";
    public static final String TYPE_XAR = "application/vnd.xara";
    public static final String TYPE_CMC = "application/vnd.cosmocaller";
    public static final String TYPE_CPIO = "application/x-cpio";
    public static final String TYPE_CLKX = "application/vnd.crick.clicker";
    public static final String TYPE_CLKK = "application/vnd.crick.clicker.keyboard";
    public static final String TYPE_CLKP = "application/vnd.crick.clicker.palette";
    public static final String TYPE_CLKT = "application/vnd.crick.clicker.template";
    public static final String TYPE_CLKW = "application/vnd.crick.clicker.wordbank";
    public static final String TYPE_WBS = "application/vnd.criticaltools.wbs+xml";
    public static final String TYPE_CRYPTONOTE = "application/vnd.rig.cryptonote";
    public static final String TYPE_CIF = "chemical/x-cif";
    public static final String TYPE_CMDF = "chemical/x-cmdf";
    public static final String TYPE_CU = "application/cu-seeme";
    public static final String TYPE_CWW = "application/prs.cww";
    public static final String TYPE_CURL = "text/vnd.curl";
    public static final String TYPE_DCURL = "text/vnd.curl.dcurl";
    public static final String TYPE_MCURL = "text/vnd.curl.mcurl";
    public static final String TYPE_SCURL = "text/vnd.curl.scurl";
    public static final String TYPE_CAR = "application/vnd.curl.car";
    public static final String TYPE_PCURL = "application/vnd.curl.pcurl";
    public static final String TYPE_CMP = "application/vnd.yellowriver-custom-menu";
    public static final String TYPE_DSSC = "application/dssc+der";
    public static final String TYPE_XDSSC = "application/dssc+xml";
    public static final String TYPE_DEB = "application/x-debian-package";
    public static final String TYPE_UVA = "audio/vnd.dece.audio";
    public static final String TYPE_UVI = "image/vnd.dece.graphic";
    public static final String TYPE_UVH = "video/vnd.dece.hd";
    public static final String TYPE_UVM = "video/vnd.dece.mobile";
    public static final String TYPE_UVU = "video/vnd.uvvu.mp4";
    public static final String TYPE_UVP = "video/vnd.dece.pd";
    public static final String TYPE_UVS = "video/vnd.dece.sd";
    public static final String TYPE_UVV = "video/vnd.dece.video";
    public static final String TYPE_DVI = "application/x-dvi";
    public static final String TYPE_SEED = "application/vnd.fdsn.seed";
    public static final String TYPE_DTB = "application/x-dtbook+xml";
    public static final String TYPE_RES = "application/x-dtbresource+xml";
    public static final String TYPE_AIT = "application/vnd.dvb.ait";
    public static final String TYPE_SVC = "application/vnd.dvb.service";
    public static final String TYPE_EOL = "audio/vnd.digital-winds";
    public static final String TYPE_DJVU = "image/vnd.djvu";
    public static final String TYPE_DTD = "application/xml-dtd";
    public static final String TYPE_MLP = "application/vnd.dolby.mlp";
    public static final String TYPE_WAD = "application/x-doom";
    public static final String TYPE_DPG = "application/vnd.dpgraph";
    public static final String TYPE_DRA = "audio/vnd.dra";
    public static final String TYPE_DFAC = "application/vnd.dreamfactory";
    public static final String TYPE_DTS = "audio/vnd.dts";
    public static final String TYPE_DTSHD = "audio/vnd.dts.hd";
    public static final String TYPE_DWG = "image/vnd.dwg";
    public static final String TYPE_GEO = "application/vnd.dynageo";
    public static final String TYPE_ES = "application/ecmascript";
    public static final String TYPE_MAG = "application/vnd.ecowin.chart";
    public static final String TYPE_MMR = "image/vnd.fujixerox.edmics-mmr";
    public static final String TYPE_RLC = "image/vnd.fujixerox.edmics-rlc";
    public static final String TYPE_EXI = "application/exi";
    public static final String TYPE_MGZ = "application/vnd.proteus.magazine";
    public static final String TYPE_EPUB = "application/epub+zip";
    public static final String TYPE_EML = "message/rfc822";
    public static final String TYPE_NML = "application/vnd.enliven";
    public static final String TYPE_XPR = "application/vnd.is-xpr";
    public static final String TYPE_XIF = "image/vnd.xiff";
    public static final String TYPE_XFDL = "application/vnd.xfdl";
    public static final String TYPE_EMMA = "application/emma+xml";
    public static final String TYPE_EZ2 = "application/vnd.ezpix-album";
    public static final String TYPE_EZ3 = "application/vnd.ezpix-package";
    public static final String TYPE_FST = "image/vnd.fst";
    public static final String TYPE_FVT = "video/vnd.fvt";
    public static final String TYPE_FBS = "image/vnd.fastbidsheet";
    public static final String TYPE_FE_LAUNCH = "application/vnd.denovo.fcselayout-link";
    public static final String TYPE_F4V = "video/x-f4v";
    public static final String TYPE_FLV = "video/x-flv";
    public static final String TYPE_FPX = "image/vnd.fpx";
    public static final String TYPE_NPX = "image/vnd.net-fpx";
    public static final String TYPE_FLX = "text/vnd.fmi.flexstor";
    public static final String TYPE_FLI = "video/x-fli";
    public static final String TYPE_FTC = "application/vnd.fluxtime.clip";
    public static final String TYPE_FDF = "application/vnd.fdf";
    public static final String TYPE_F = "text/x-fortran";
    public static final String TYPE_MIF = "application/vnd.mif";
    public static final String TYPE_FM = "application/vnd.framemaker";
    public static final String TYPE_FH = "image/x-freehand";
    public static final String TYPE_FSC = "application/vnd.fsc.weblaunch";
    public static final String TYPE_FNC = "application/vnd.frogans.fnc";
    public static final String TYPE_LTF = "application/vnd.frogans.ltf";
    public static final String TYPE_DDD = "application/vnd.fujixerox.ddd";
    public static final String TYPE_XDW = "application/vnd.fujixerox.docuworks";
    public static final String TYPE_XBD = "application/vnd.fujixerox.docuworks.binder";
    public static final String TYPE_OAS = "application/vnd.fujitsu.oasys";
    public static final String TYPE_OA2 = "application/vnd.fujitsu.oasys2";
    public static final String TYPE_OA3 = "application/vnd.fujitsu.oasys3";
    public static final String TYPE_FG5 = "application/vnd.fujitsu.oasysgp";
    public static final String TYPE_BH2 = "application/vnd.fujitsu.oasysprs";
    public static final String TYPE_SPL = "application/x-futuresplash";
    public static final String TYPE_FZS = "application/vnd.fuzzysheet";
    public static final String TYPE_G3 = "image/g3fax";
    public static final String TYPE_GMX = "application/vnd.gmx";
    public static final String TYPE_GTW = "model/vnd.gtw";
    public static final String TYPE_TXD = "application/vnd.genomatix.tuxedo";
    public static final String TYPE_GGB = "application/vnd.geogebra.file";
    public static final String TYPE_GGT = "application/vnd.geogebra.tool";
    public static final String TYPE_GDL = "model/vnd.gdl";
    public static final String TYPE_GEX = "application/vnd.geometry-explorer";
    public static final String TYPE_GXT = "application/vnd.geonext";
    public static final String TYPE_G2W = "application/vnd.geoplan";
    public static final String TYPE_G3W = "application/vnd.geospace";
    public static final String TYPE_GSF = "application/x-font-ghostscript";
    public static final String TYPE_BDF = "application/x-font-bdf";
    public static final String TYPE_GTAR = "application/x-gtar";
    public static final String TYPE_TEXINFO = "application/x-texinfo";
    public static final String TYPE_GNUMERIC = "application/x-gnumeric";
    public static final String TYPE_KML = "application/vnd.google-earth.kml+xml";
    public static final String TYPE_KMZ = "application/vnd.google-earth.kmz";
    public static final String TYPE_GQF = "application/vnd.grafeq";
    public static final String TYPE_GIF = "image/gif";
    public static final String TYPE_GV = "text/vnd.graphviz";
    public static final String TYPE_GAC = "application/vnd.groove-account";
    public static final String TYPE_GHF = "application/vnd.groove-help";
    public static final String TYPE_GIM = "application/vnd.groove-identity-message";
    public static final String TYPE_GRV = "application/vnd.groove-injector";
    public static final String TYPE_GTM = "application/vnd.groove-tool-message";
    public static final String TYPE_TPL = "application/vnd.groove-tool-template";
    public static final String TYPE_VCG = "application/vnd.groove-vcard";
    public static final String TYPE_H261 = "video/h261";
    public static final String TYPE_H263 = "video/h263";
    public static final String TYPE_H264 = "video/h264";
    public static final String TYPE_HPID = "application/vnd.hp-hpid";
    public static final String TYPE_HPS = "application/vnd.hp-hps";
    public static final String TYPE_HDF = "application/x-hdf";
    public static final String TYPE_RIP = "audio/vnd.rip";
    public static final String TYPE_HBCI = "application/vnd.hbci";
    public static final String TYPE_JLT = "application/vnd.hp-jlyt";
    public static final String TYPE_PCL = "application/vnd.hp-pcl";
    public static final String TYPE_HPGL = "application/vnd.hp-hpgl";
    public static final String TYPE_HVS = "application/vnd.yamaha.hv-script";
    public static final String TYPE_HVD = "application/vnd.yamaha.hv-dic";
    public static final String TYPE_HVP = "application/vnd.yamaha.hv-voice";
    // public static final String TYPE_SFD-HDSTX= "application/vnd.hydrostatix.sof-data";
    public static final String TYPE_STK = "application/hyperstudio";
    public static final String TYPE_HAL = "application/vnd.hal+xml";
    public static final String TYPE_HTML = "text/html";
    public static final String TYPE_IRM = "application/vnd.ibm.rights-management";
    public static final String TYPE_SC = "application/vnd.ibm.secure-container";
    public static final String TYPE_ICS = "text/calendar";
    public static final String TYPE_ICC = "application/vnd.iccprofile";
    public static final String TYPE_ICO = "image/x-icon";
    public static final String TYPE_IGL = "application/vnd.igloader";
    public static final String TYPE_IEF = "image/ief";
    public static final String TYPE_IVP = "application/vnd.immervision-ivp";
    public static final String TYPE_IVU = "application/vnd.immervision-ivu";
    public static final String TYPE_RIF = "application/reginfo+xml";
    public static final String TYPE_3DML = "text/vnd.in3d.3dml";
    public static final String TYPE_SPOT = "text/vnd.in3d.spot";
    public static final String TYPE_IGS = "model/iges";
    public static final String TYPE_I2G = "application/vnd.intergeo";
    public static final String TYPE_CDY = "application/vnd.cinderella";
    public static final String TYPE_XPW = "application/vnd.intercon.formnet";
    public static final String TYPE_FCS = "application/vnd.isac.fcs";
    public static final String TYPE_IPFIX = "application/ipfix";
    public static final String TYPE_CER = "application/pkix-cert";
    public static final String TYPE_PKI = "application/pkixcmp";
    public static final String TYPE_CRL = "application/pkix-crl";
    public static final String TYPE_PKIPATH = "application/pkix-pkipath";
    public static final String TYPE_IGM = "application/vnd.insors.igm";
    public static final String TYPE_RCPROFILE = "application/vnd.ipunplugged.rcprofile";
    public static final String TYPE_IRP = "application/vnd.irepository.package+xml";
    public static final String TYPE_JAD = "text/vnd.sun.j2me.app-descriptor";
    public static final String TYPE_JAR = "application/java-archive";
    public static final String TYPE_CLASS = "application/java-vm";
    public static final String TYPE_JNLP = "application/x-java-jnlp-file";
    public static final String TYPE_SER = "application/java-serialized-object";
    public static final String TYPE_JAVA = "text/x-java-source,java";
    public static final String TYPE_JS = "application/javascript";
    public static final String TYPE_JSON = "application/json";
    public static final String TYPE_JODA = "application/vnd.joost.joda-archive";
    public static final String TYPE_JPM = "video/jpm";
    public static final String TYPE_JPEG = "image/jpeg";
    public static final String TYPE_PJPEG = "image/pjpeg";
    public static final String TYPE_JPGV = "video/jpeg";
    public static final String TYPE_KTZ = "application/vnd.kahootz";
    public static final String TYPE_MMD = "application/vnd.chipnuts.karaoke-mmd";
    public static final String TYPE_KARBON = "application/vnd.kde.karbon";
    public static final String TYPE_CHRT = "application/vnd.kde.kchart";
    public static final String TYPE_KFO = "application/vnd.kde.kformula";
    public static final String TYPE_FLW = "application/vnd.kde.kivio";
    public static final String TYPE_KON = "application/vnd.kde.kontour";
    public static final String TYPE_KPR = "application/vnd.kde.kpresenter";
    public static final String TYPE_KSP = "application/vnd.kde.kspread";
    public static final String TYPE_KWD = "application/vnd.kde.kword";
    public static final String TYPE_HTKE = "application/vnd.kenameaapp";
    public static final String TYPE_KIA = "application/vnd.kidspiration";
    public static final String TYPE_KNE = "application/vnd.kinar";
    public static final String TYPE_SSE = "application/vnd.kodak-descriptor";
    public static final String TYPE_LASXML = "application/vnd.las.las+xml";
    public static final String TYPE_LATEX = "application/x-latex";
    public static final String TYPE_LBD = "application/vnd.llamagraphics.life-balance.desktop";
    public static final String TYPE_LBE = "application/vnd.llamagraphics.life-balance.exchange+xml";
    public static final String TYPE_JAM = "application/vnd.jam";
    public static final String TYPE_APR = "application/vnd.lotus-approach";
    public static final String TYPE_PRE = "application/vnd.lotus-freelance";
    public static final String TYPE_NSF = "application/vnd.lotus-notes";
    public static final String TYPE_ORG = "application/vnd.lotus-organizer";
    public static final String TYPE_SCM = "application/vnd.lotus-screencam";
    public static final String TYPE_LWP = "application/vnd.lotus-wordpro";
    public static final String TYPE_LVP = "audio/vnd.lucent.voice";
    public static final String TYPE_M3U = "audio/x-mpegurl";
    public static final String TYPE_M4V = "video/x-m4v";
    public static final String TYPE_HQX = "application/mac-binhex40";
    public static final String TYPE_PORTPKG = "application/vnd.macports.portpkg";
    public static final String TYPE_MGP = "application/vnd.osgeo.mapguide.package";
    public static final String TYPE_MRC = "application/marc";
    public static final String TYPE_MRCX = "application/marcxml+xml";
    public static final String TYPE_MXF = "application/mxf";
    public static final String TYPE_NBP = "application/vnd.wolfram.player";
    public static final String TYPE_MA = "application/mathematica";
    public static final String TYPE_MATHML = "application/mathml+xml";
    public static final String TYPE_MBOX = "application/mbox";
    public static final String TYPE_MC1 = "application/vnd.medcalcdata";
    public static final String TYPE_MSCML = "application/mediaservercontrol+xml";
    public static final String TYPE_CDKEY = "application/vnd.mediastation.cdkey";
    public static final String TYPE_MWF = "application/vnd.mfer";
    public static final String TYPE_MFM = "application/vnd.mfmp";
    public static final String TYPE_MSH = "model/mesh";
    public static final String TYPE_MADS = "application/mads+xml";
    public static final String TYPE_METS = "application/mets+xml";
    public static final String TYPE_MODS = "application/mods+xml";
    public static final String TYPE_META4 = "application/metalink4+xml";
    public static final String TYPE_MCD = "application/vnd.mcd";
    public static final String TYPE_FLO = "application/vnd.micrografx.flo";
    public static final String TYPE_IGX = "application/vnd.micrografx.igx";
    public static final String TYPE_ES3 = "application/vnd.eszigno3+xml";
    public static final String TYPE_MDB = "application/x-msaccess";
    public static final String TYPE_ASF = "video/x-ms-asf";
    public static final String TYPE_EXE = "application/x-msdownload";
    public static final String TYPE_CIL = "application/vnd.ms-artgalry";
    public static final String TYPE_CAB = "application/vnd.ms-cab-compressed";
    public static final String TYPE_IMS = "application/vnd.ms-ims";
    public static final String TYPE_APPLICATION = "application/x-ms-application";
    public static final String TYPE_CLP = "application/x-msclip";
    public static final String TYPE_MDI = "image/vnd.ms-modi";
    public static final String TYPE_EOT = "application/vnd.ms-fontobject";
    public static final String TYPE_XLS = "application/vnd.ms-excel";
    public static final String TYPE_XLAM = "application/vnd.ms-excel.addin.macroenabled.12";
    public static final String TYPE_XLSB = "application/vnd.ms-excel.sheet.binary.macroenabled.12";
    public static final String TYPE_XLTM = "application/vnd.ms-excel.template.macroenabled.12";
    public static final String TYPE_XLSM = "application/vnd.ms-excel.sheet.macroenabled.12";
    public static final String TYPE_CHM = "application/vnd.ms-htmlhelp";
    public static final String TYPE_CRD = "application/x-mscardfile";
    public static final String TYPE_LRM = "application/vnd.ms-lrm";
    public static final String TYPE_MVB = "application/x-msmediaview";
    public static final String TYPE_MNY = "application/x-msmoney";
    public static final String TYPE_PPTX =
            "application/vnd.openxmlformats-officedocument.presentationml.presentation";
    public static final String TYPE_SLDX =
            "application/vnd.openxmlformats-officedocument.presentationml.slide";
    public static final String TYPE_PPSX =
            "application/vnd.openxmlformats-officedocument.presentationml.slideshow";
    public static final String TYPE_POTX =
            "application/vnd.openxmlformats-officedocument.presentationml.template";
    public static final String TYPE_XLSX =
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
    public static final String TYPE_XLTX =
            "application/vnd.openxmlformats-officedocument.spreadsheetml.template";
    public static final String TYPE_DOCX =
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
    public static final String TYPE_DOTX =
            "application/vnd.openxmlformats-officedocument.wordprocessingml.template";
    public static final String TYPE_OBD = "application/x-msbinder";
    public static final String TYPE_THMX = "application/vnd.ms-officetheme";
    public static final String TYPE_ONETOC = "application/onenote";
    public static final String TYPE_PYA = "audio/vnd.ms-playready.media.pya";
    public static final String TYPE_PYV = "video/vnd.ms-playready.media.pyv";
    public static final String TYPE_PPT = "application/vnd.ms-powerpoint";
    public static final String TYPE_PPAM = "application/vnd.ms-powerpoint.addin.macroenabled.12";
    public static final String TYPE_SLDM = "application/vnd.ms-powerpoint.slide.macroenabled.12";
    public static final String TYPE_PPTM =
            "application/vnd.ms-powerpoint.presentation.macroenabled.12";
    public static final String TYPE_PPSM = "application/vnd.ms-powerpoint.slideshow.macroenabled.12";
    public static final String TYPE_POTM = "application/vnd.ms-powerpoint.template.macroenabled.12";
    public static final String TYPE_MPP = "application/vnd.ms-project";
    public static final String TYPE_PUB = "application/x-mspublisher";
    public static final String TYPE_SCD = "application/x-msschedule";
    public static final String TYPE_XAP = "application/x-silverlight-app";
    public static final String TYPE_STL = "application/vnd.ms-pki.stl";
    public static final String TYPE_CAT = "application/vnd.ms-pki.seccat";
    public static final String TYPE_VSD = "application/vnd.visio";
    public static final String TYPE_VSDX = "application/vnd.visio2013";
    public static final String TYPE_WM = "video/x-ms-wm";
    public static final String TYPE_WMA = "audio/x-ms-wma";
    public static final String TYPE_WAX = "audio/x-ms-wax";
    public static final String TYPE_WMX = "video/x-ms-wmx";
    public static final String TYPE_WMD = "application/x-ms-wmd";
    public static final String TYPE_WPL = "application/vnd.ms-wpl";
    public static final String TYPE_WMZ = "application/x-ms-wmz";
    public static final String TYPE_WMV = "video/x-ms-wmv";
    public static final String TYPE_WVX = "video/x-ms-wvx";
    public static final String TYPE_WMF = "application/x-msmetafile";
    public static final String TYPE_TRM = "application/x-msterminal";
    public static final String TYPE_DOC = "application/msword";
    public static final String TYPE_DOCM = "application/vnd.ms-word.document.macroenabled.12";
    public static final String TYPE_DOTM = "application/vnd.ms-word.template.macroenabled.12";
    public static final String TYPE_WRI = "application/x-mswrite";
    public static final String TYPE_WPS = "application/vnd.ms-works";
    public static final String TYPE_XBAP = "application/x-ms-xbap";
    public static final String TYPE_XPS = "application/vnd.ms-xpsdocument";
    public static final String TYPE_MID = "audio/midi";
    public static final String TYPE_MPY = "application/vnd.ibm.minipay";
    public static final String TYPE_AFP = "application/vnd.ibm.modcap";
    public static final String TYPE_RMS = "application/vnd.jcp.javame.midlet-rms";
    public static final String TYPE_TMO = "application/vnd.tmobile-livetv";
    public static final String TYPE_PRC = "application/x-mobipocket-ebook";
    public static final String TYPE_MBK = "application/vnd.mobius.mbk";
    public static final String TYPE_DIS = "application/vnd.mobius.dis";
    public static final String TYPE_PLC = "application/vnd.mobius.plc";
    public static final String TYPE_MQY = "application/vnd.mobius.mqy";
    public static final String TYPE_MSL = "application/vnd.mobius.msl";
    public static final String TYPE_TXF = "application/vnd.mobius.txf";
    public static final String TYPE_DAF = "application/vnd.mobius.daf";
    public static final String TYPE_FLY = "text/vnd.fly";
    public static final String TYPE_MPC = "application/vnd.mophun.certificate";
    public static final String TYPE_MPN = "application/vnd.mophun.application";
    public static final String TYPE_MJ2 = "video/mj2";
    public static final String TYPE_MPGA = "audio/mpeg";
    public static final String TYPE_MXU = "video/vnd.mpegurl";
    public static final String TYPE_MPEG = "video/mpeg";
    public static final String TYPE_M21 = "application/mp21";
    public static final String TYPE_MP4A = "audio/mp4";
    public static final String TYPE_MP4 = "video/mp4";
    // public static final String TYPE_MP4= "application/mp4";
    public static final String TYPE_M3U8 = "application/vnd.apple.mpegurl";
    public static final String TYPE_MUS = "application/vnd.musician";
    public static final String TYPE_MSTY = "application/vnd.muvee.style";
    public static final String TYPE_MXML = "application/xv+xml";
    public static final String TYPE_NGDAT = "application/vnd.nokia.n-gage.data";
    // public static final String TYPE_N-GAGE= "application/vnd.nokia.n-gage.symbian.install";
    public static final String TYPE_NCX = "application/x-dtbncx+xml";
    public static final String TYPE_NC = "application/x-netcdf";
    public static final String TYPE_NLU = "application/vnd.neurolanguage.nlu";
    public static final String TYPE_DNA = "application/vnd.dna";
    public static final String TYPE_NND = "application/vnd.noblenet-directory";
    public static final String TYPE_NNS = "application/vnd.noblenet-sealer";
    public static final String TYPE_NNW = "application/vnd.noblenet-web";
    public static final String TYPE_RPST = "application/vnd.nokia.radio-preset";
    public static final String TYPE_RPSS = "application/vnd.nokia.radio-presets";
    public static final String TYPE_N3 = "text/n3";
    public static final String TYPE_EDM = "application/vnd.novadigm.edm";
    public static final String TYPE_EDX = "application/vnd.novadigm.edx";
    public static final String TYPE_EXT = "application/vnd.novadigm.ext";
    public static final String TYPE_GPH = "application/vnd.flographit";
    public static final String TYPE_ECELP4800 = "audio/vnd.nuera.ecelp4800";
    public static final String TYPE_ECELP7470 = "audio/vnd.nuera.ecelp7470";
    public static final String TYPE_ECELP9600 = "audio/vnd.nuera.ecelp9600";
    public static final String TYPE_ODA = "application/oda";
    public static final String TYPE_OGX = "application/ogg";
    public static final String TYPE_OGA = "audio/ogg";
    public static final String TYPE_OGV = "video/ogg";
    public static final String TYPE_DD2 = "application/vnd.oma.dd2+xml";
    public static final String TYPE_OTH = "application/vnd.oasis.opendocument.text-web";
    public static final String TYPE_OPF = "application/oebps-package+xml";
    public static final String TYPE_QBO = "application/vnd.intu.qbo";
    public static final String TYPE_OXT = "application/vnd.openofficeorg.extension";
    public static final String TYPE_OSF = "application/vnd.yamaha.openscoreformat";
    public static final String TYPE_WEBA = "audio/webm";
    public static final String TYPE_WEBM = "video/webm";
    public static final String TYPE_ODC = "application/vnd.oasis.opendocument.chart";
    public static final String TYPE_OTC = "application/vnd.oasis.opendocument.chart-template";
    public static final String TYPE_ODB = "application/vnd.oasis.opendocument.database";
    public static final String TYPE_ODF = "application/vnd.oasis.opendocument.formula";
    public static final String TYPE_ODFT = "application/vnd.oasis.opendocument.formula-template";
    public static final String TYPE_ODG = "application/vnd.oasis.opendocument.graphics";
    public static final String TYPE_OTG = "application/vnd.oasis.opendocument.graphics-template";
    public static final String TYPE_ODI = "application/vnd.oasis.opendocument.image";
    public static final String TYPE_OTI = "application/vnd.oasis.opendocument.image-template";
    public static final String TYPE_ODP = "application/vnd.oasis.opendocument.presentation";
    public static final String TYPE_OTP = "application/vnd.oasis.opendocument.presentation-template";
    public static final String TYPE_ODS = "application/vnd.oasis.opendocument.spreadsheet";
    public static final String TYPE_OTS = "application/vnd.oasis.opendocument.spreadsheet-template";
    public static final String TYPE_ODT = "application/vnd.oasis.opendocument.text";
    public static final String TYPE_ODM = "application/vnd.oasis.opendocument.text-master";
    public static final String TYPE_OTT = "application/vnd.oasis.opendocument.text-template";
    public static final String TYPE_KTX = "image/ktx";
    public static final String TYPE_SXC = "application/vnd.sun.xml.calc";
    public static final String TYPE_STC = "application/vnd.sun.xml.calc.template";
    public static final String TYPE_SXD = "application/vnd.sun.xml.draw";
    public static final String TYPE_STD = "application/vnd.sun.xml.draw.template";
    public static final String TYPE_SXI = "application/vnd.sun.xml.impress";
    public static final String TYPE_STI = "application/vnd.sun.xml.impress.template";
    public static final String TYPE_SXM = "application/vnd.sun.xml.math";
    public static final String TYPE_SXW = "application/vnd.sun.xml.writer";
    public static final String TYPE_SXG = "application/vnd.sun.xml.writer.global";
    public static final String TYPE_STW = "application/vnd.sun.xml.writer.template";
    public static final String TYPE_OTF = "application/x-font-otf";
    public static final String TYPE_OSFPVG = "application/vnd.yamaha.openscoreformat.osfpvg+xml";
    public static final String TYPE_DP = "application/vnd.osgi.dp";
    public static final String TYPE_PDB = "application/vnd.palm";
    public static final String TYPE_P = "text/x-pascal";
    public static final String TYPE_PAW = "application/vnd.pawaafile";
    public static final String TYPE_PCLXL = "application/vnd.hp-pclxl";
    public static final String TYPE_EFIF = "application/vnd.picsel";
    public static final String TYPE_PCX = "image/x-pcx";
    public static final String TYPE_PSD = "image/vnd.adobe.photoshop";
    public static final String TYPE_PRF = "application/pics-rules";
    public static final String TYPE_PIC = "image/x-pict";
    public static final String TYPE_CHAT = "application/x-chat";
    public static final String TYPE_P10 = "application/pkcs10";
    public static final String TYPE_P12 = "application/x-pkcs12";
    public static final String TYPE_P7M = "application/pkcs7-mime";
    public static final String TYPE_P7S = "application/pkcs7-signature";
    public static final String TYPE_P7R = "application/x-pkcs7-certreqresp";
    public static final String TYPE_P7B = "application/x-pkcs7-certificates";
    public static final String TYPE_P8 = "application/pkcs8";
    public static final String TYPE_PLF = "application/vnd.pocketlearn";
    public static final String TYPE_PNM = "image/x-portable-anymap";
    public static final String TYPE_PBM = "image/x-portable-bitmap";
    public static final String TYPE_PCF = "application/x-font-pcf";
    public static final String TYPE_PFR = "application/font-tdpfr";
    public static final String TYPE_PGN = "application/x-chess-pgn";
    public static final String TYPE_PGM = "image/x-portable-graymap";
    public static final String TYPE_PNG = "image/png";
    public static final String TYPE_PPM = "image/x-portable-pixmap";
    public static final String TYPE_PSKCXML = "application/pskc+xml";
    public static final String TYPE_PML = "application/vnd.ctc-posml";
    public static final String TYPE_AI = "application/postscript";
    public static final String TYPE_PFA = "application/x-font-type1";
    public static final String TYPE_PBD = "application/vnd.powerbuilder6";
    public static final String TYPE_PGP = "application/pgp-encrypted";
    public static final String TYPE_BOX = "application/vnd.previewsystems.box";
    public static final String TYPE_PTID = "application/vnd.pvi.ptid1";
    public static final String TYPE_PLS = "application/pls+xml";
    public static final String TYPE_STR = "application/vnd.pg.format";
    public static final String TYPE_EI6 = "application/vnd.pg.osasli";
    public static final String TYPE_DSC = "text/prs.lines.tag";
    public static final String TYPE_PSF = "application/x-font-linux-psf";
    public static final String TYPE_QPS = "application/vnd.publishare-delta-tree";
    public static final String TYPE_WG = "application/vnd.pmi.widget";
    public static final String TYPE_QXD = "application/vnd.quark.quarkxpress";
    public static final String TYPE_ESF = "application/vnd.epson.esf";
    public static final String TYPE_MSF = "application/vnd.epson.msf";
    public static final String TYPE_SSF = "application/vnd.epson.ssf";
    public static final String TYPE_QAM = "application/vnd.epson.quickanime";
    public static final String TYPE_QFX = "application/vnd.intu.qfx";
    public static final String TYPE_QT = "video/quicktime";
    public static final String TYPE_RAR = "application/x-rar-compressed";
    public static final String TYPE_RAM = "audio/x-pn-realaudio";
    public static final String TYPE_RMP = "audio/x-pn-realaudio-plugin";
    public static final String TYPE_RSD = "application/rsd+xml";
    public static final String TYPE_RM = "application/vnd.rn-realmedia";
    public static final String TYPE_BED = "application/vnd.realvnc.bed";
    public static final String TYPE_MXL = "application/vnd.recordare.musicxml";
    public static final String TYPE_MUSICXML = "application/vnd.recordare.musicxml+xml";
    public static final String TYPE_RNC = "application/relax-ng-compact-syntax";
    public static final String TYPE_RDZ = "application/vnd.data-vision.rdz";
    public static final String TYPE_RDF = "application/rdf+xml";
    public static final String TYPE_RP9 = "application/vnd.cloanto.rp9";
    public static final String TYPE_JISP = "application/vnd.jisp";
    public static final String TYPE_RTF = "application/rtf";
    public static final String TYPE_RTX = "text/richtext";
    public static final String TYPE_LINK66 = "application/vnd.route66.link66+xml";
    public static final String TYPE_RSS = "application/rss+xml";
    public static final String TYPE_SHF = "application/shf+xml";
    public static final String TYPE_ST = "application/vnd.sailingtracker.track";
    public static final String TYPE_SVG = "image/svg+xml";
    public static final String TYPE_SUS = "application/vnd.sus-calendar";
    public static final String TYPE_SRU = "application/sru+xml";
    public static final String TYPE_SETPAY = "application/set-payment-initiation";
    public static final String TYPE_SETREG = "application/set-registration-initiation";
    public static final String TYPE_SEMA = "application/vnd.sema";
    public static final String TYPE_SEMD = "application/vnd.semd";
    public static final String TYPE_SEMF = "application/vnd.semf";
    public static final String TYPE_SEE = "application/vnd.seemail";
    public static final String TYPE_SNF = "application/x-font-snf";
    public static final String TYPE_SPQ = "application/scvp-vp-request";
    public static final String TYPE_SPP = "application/scvp-vp-response";
    public static final String TYPE_SCQ = "application/scvp-cv-request";
    public static final String TYPE_SCS = "application/scvp-cv-response";
    public static final String TYPE_SDP = "application/sdp";
    public static final String TYPE_ETX = "text/x-setext";
    public static final String TYPE_MOVIE = "video/x-sgi-movie";
    public static final String TYPE_IFM = "application/vnd.shana.informed.formdata";
    public static final String TYPE_ITP = "application/vnd.shana.informed.formtemplate";
    public static final String TYPE_IIF = "application/vnd.shana.informed.interchange";
    public static final String TYPE_IPK = "application/vnd.shana.informed.package";
    public static final String TYPE_TFI = "application/thraud+xml";
    public static final String TYPE_SHAR = "application/x-shar";
    public static final String TYPE_RGB = "image/x-rgb";
    public static final String TYPE_SLT = "application/vnd.epson.salt";
    public static final String TYPE_ASO = "application/vnd.accpac.simply.aso";
    public static final String TYPE_IMP = "application/vnd.accpac.simply.imp";
    public static final String TYPE_TWD = "application/vnd.simtech-mindmapper";
    public static final String TYPE_CSP = "application/vnd.commonspace";
    public static final String TYPE_SAF = "application/vnd.yamaha.smaf-audio";
    public static final String TYPE_MMF = "application/vnd.smaf";
    public static final String TYPE_SPF = "application/vnd.yamaha.smaf-phrase";
    public static final String TYPE_TEACHER = "application/vnd.smart.teacher";
    public static final String TYPE_SVD = "application/vnd.svd";
    public static final String TYPE_RQ = "application/sparql-query";
    public static final String TYPE_SRX = "application/sparql-results+xml";
    public static final String TYPE_GRAM = "application/srgs";
    public static final String TYPE_GRXML = "application/srgs+xml";
    public static final String TYPE_SSML = "application/ssml+xml";
    public static final String TYPE_SKP = "application/vnd.koan";
    public static final String TYPE_SGML = "text/sgml";
    public static final String TYPE_SDC = "application/vnd.stardivision.calc";
    public static final String TYPE_SDA = "application/vnd.stardivision.draw";
    public static final String TYPE_SDD = "application/vnd.stardivision.impress";
    public static final String TYPE_SMF = "application/vnd.stardivision.math";
    public static final String TYPE_SDW = "application/vnd.stardivision.writer";
    public static final String TYPE_SGL = "application/vnd.stardivision.writer-global";
    public static final String TYPE_SM = "application/vnd.stepmania.stepchart";
    public static final String TYPE_SIT = "application/x-stuffit";
    public static final String TYPE_SITX = "application/x-stuffitx";
    public static final String TYPE_SDKM = "application/vnd.solent.sdkm+xml";
    public static final String TYPE_XO = "application/vnd.olpc-sugar";
    public static final String TYPE_AU = "audio/basic";
    public static final String TYPE_WQD = "application/vnd.wqd";
    public static final String TYPE_SIS = "application/vnd.symbian.install";
    public static final String TYPE_SMI = "application/smil+xml";
    public static final String TYPE_XSM = "application/vnd.syncml+xml";
    public static final String TYPE_BDM = "application/vnd.syncml.dm+wbxml";
    public static final String TYPE_XDM = "application/vnd.syncml.dm+xml";
    public static final String TYPE_SV4CPIO = "application/x-sv4cpio";
    public static final String TYPE_SV4CRC = "application/x-sv4crc";
    public static final String TYPE_SBML = "application/sbml+xml";
    public static final String TYPE_TSV = "text/tab-separated-values";
    public static final String TYPE_TIFF = "image/tiff";
    public static final String TYPE_TAO = "application/vnd.tao.intent-module-archive";
    public static final String TYPE_TAR = "application/x-tar";
    public static final String TYPE_TCL = "application/x-tcl";
    public static final String TYPE_TEX = "application/x-tex";
    public static final String TYPE_TFM = "application/x-tex-tfm";
    public static final String TYPE_TEI = "application/tei+xml";
    public static final String TYPE_TXT = "text/plain";
    public static final String TYPE_DXP = "application/vnd.spotfire.dxp";
    public static final String TYPE_SFS = "application/vnd.spotfire.sfs";
    public static final String TYPE_TSD = "application/timestamped-data";
    public static final String TYPE_TPT = "application/vnd.trid.tpt";
    public static final String TYPE_MXS = "application/vnd.triscape.mxs";
    public static final String TYPE_T = "text/troff";
    public static final String TYPE_TRA = "application/vnd.trueapp";
    public static final String TYPE_TTF = "application/x-font-ttf";
    public static final String TYPE_TTL = "text/turtle";
    public static final String TYPE_UMJ = "application/vnd.umajin";
    public static final String TYPE_UOML = "application/vnd.uoml+xml";
    public static final String TYPE_UNITYWEB = "application/vnd.unity";
    public static final String TYPE_UFD = "application/vnd.ufdl";
    public static final String TYPE_URI = "text/uri-list";
    public static final String TYPE_UTZ = "application/vnd.uiq.theme";
    public static final String TYPE_USTAR = "application/x-ustar";
    public static final String TYPE_UU = "text/x-uuencode";
    public static final String TYPE_VCS = "text/x-vcalendar";
    public static final String TYPE_VCF = "text/x-vcard";
    public static final String TYPE_VCD = "application/x-cdlink";
    public static final String TYPE_VSF = "application/vnd.vsf";
    public static final String TYPE_WRL = "model/vrml";
    public static final String TYPE_VCX = "application/vnd.vcx";
    public static final String TYPE_MTS = "model/vnd.mts";
    public static final String TYPE_VTU = "model/vnd.vtu";
    public static final String TYPE_VIS = "application/vnd.visionary";
    public static final String TYPE_VIV = "video/vnd.vivo";
    public static final String TYPE_CCXML = "application/ccxml+xml,";
    public static final String TYPE_VXML = "application/voicexml+xml";
    public static final String TYPE_SRC = "application/x-wais-source";
    public static final String TYPE_WBXML = "application/vnd.wap.wbxml";
    public static final String TYPE_WBMP = "image/vnd.wap.wbmp";
    public static final String TYPE_WAV = "audio/x-wav";
    public static final String TYPE_DAVMOUNT = "application/davmount+xml";
    public static final String TYPE_WOFF = "application/x-font-woff";
    public static final String TYPE_WSPOLICY = "application/wspolicy+xml";
    public static final String TYPE_WEBP = "image/webp";
    public static final String TYPE_WTB = "application/vnd.webturbo";
    public static final String TYPE_WGT = "application/widget";
    public static final String TYPE_HLP = "application/winhlp";
    public static final String TYPE_WML = "text/vnd.wap.wml";
    public static final String TYPE_WMLS = "text/vnd.wap.wmlscript";
    public static final String TYPE_WMLSC = "application/vnd.wap.wmlscriptc";
    public static final String TYPE_WPD = "application/vnd.wordperfect";
    public static final String TYPE_STF = "application/vnd.wt.stf";
    public static final String TYPE_WSDL = "application/wsdl+xml";
    public static final String TYPE_XBM = "image/x-xbitmap";
    public static final String TYPE_XPM = "image/x-xpixmap";
    public static final String TYPE_XWD = "image/x-xwindowdump";
    public static final String TYPE_DER = "application/x-x509-ca-cert";
    public static final String TYPE_FIG = "application/x-xfig";
    public static final String TYPE_XHTML = "application/xhtml+xml";
    public static final String TYPE_XML = "application/xml";
    public static final String TYPE_XDF = "application/xcap-diff+xml";
    public static final String TYPE_XENC = "application/xenc+xml";
    public static final String TYPE_XER = "application/patch-ops-error+xml";
    public static final String TYPE_RL = "application/resource-lists+xml";
    public static final String TYPE_RS = "application/rls-services+xml";
    public static final String TYPE_RLD = "application/resource-lists-diff+xml";
    public static final String TYPE_XSLT = "application/xslt+xml";
    public static final String TYPE_XOP = "application/xop+xml";
    public static final String TYPE_XPI = "application/x-xpinstall";
    public static final String TYPE_XSPF = "application/xspf+xml";
    public static final String TYPE_XUL = "application/vnd.mozilla.xul+xml";
    public static final String TYPE_XYZ = "chemical/x-xyz";
    public static final String TYPE_YAML = "text/yaml";
    public static final String TYPE_YANG = "application/yang";
    public static final String TYPE_YIN = "application/yin+xml";
    public static final String TYPE_ZIR = "application/vnd.zul";
    public static final String TYPE_ZIP = "application/zip";
    public static final String TYPE_ZMM = "application/vnd.handheld-entertainment+xml";
    public static final String TYPE_ZAZ = "application/vnd.zzazz.deck+xml";
    private String fileName;
    private String mimeType;
    private byte[] data;
    private Map<String, String> headers;

    public static BlobObject jsonUTF8(String json) {
        BlobObject blob = new BlobObject();
        blob.setMimeType("application/json;charset=UTF-8");
        blob.setData(json.getBytes(StandardCharsets.UTF_8));
        blob.setHeaders(MapUtil.of("Charset", "UTF-8"));
        return blob;
    }

    public static BlobObject plainUTF8Text(String text) {
        return plainText(text, StandardCharsets.UTF_8);
    }

    public static BlobObject plainGBKText(String text) {
        return plainText(text, CharsetUtil.CHARSET_GBK);
    }

    protected static BlobObject plainText(String text, Charset charset) {
        BlobObject blob = new BlobObject();
        blob.setMimeType(BlobObject.TYPE_TXT + "; charset=" + charset.name());
        blob.setData(text.getBytes(charset));
        return blob;
    }

    protected static BlobObject binaryFile(String fileName, byte[] content, String mimeType) {
        BlobObject blob = new BlobObject();
        blob.setMimeType(mimeType);
        blob.setFileName(fileName);
        blob.setData(content);
        return blob;
    }

    public static BlobObject binaryStream(byte[] content, String mimeType) {
        BlobObject blob = new BlobObject();
        blob.setMimeType(mimeType);
        blob.setData(content);
        return blob;
    }

    public static BlobObject jpgImage(byte[] content) {
        return binaryStream(content, BlobObject.TYPE_JPEG);
    }

    public static BlobObject pngImage(byte[] content) {
        return binaryStream(content, BlobObject.TYPE_PNG);
    }

    public static BlobObject pdfFile(String fileName, byte[] content) {
        return binaryFile(fileName, content, BlobObject.TYPE_PDF);
    }

    public static BlobObject textFile(String fileName, byte[] content) {
        return binaryFile(fileName, content, BlobObject.TYPE_TXT);
    }

    public static BlobObject excelFile(String fileName, byte[] content) {
        return binaryFile(fileName, content, BlobObject.TYPE_XLSX);
    }

    public static BlobObject jpgFile(String fileName, byte[] content) {
        return binaryFile(fileName, content, BlobObject.TYPE_JPEG);
    }

    public static BlobObject pngFile(String fileName, byte[] content) {
        return binaryFile(fileName, content, BlobObject.TYPE_PNG);
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
        String headerName = "Content-Disposition";
        String attachmentHeader =
                StrUtil.format(
                        "attachment; filename*=UTF-8''{}",
                        URLEncodeUtil.encode(fileName, CharsetUtil.CHARSET_UTF_8));
        this.addHeader(headerName, attachmentHeader);
        this.addHeader("X-File-Name-Base64", Base64.encode(fileName));
    }

    public BlobObject packPlainText(String content) {
        BlobObject blob = new BlobObject();
        blob.setMimeType(BlobObject.TYPE_TXT + "; charset=UTF-8");

        if (content == null) {
            return blob;
        }
        blob.setData(content.getBytes());

        return blob;
    }

    public String getMimeType() {
        return mimeType;
    }

    public void setMimeType(String mimeType) {
        this.mimeType = mimeType;
        addHeader("Content-Type", mimeType);
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] data) {
        this.data = data;
        addHeader("Content-Length", String.valueOf(data.length));
    }

    public Map<String, String> getHeaders() {
        if (headers == null) {
            headers = new HashMap<String, String>();
        }
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public BlobObject addHeader(String name, String value) {
        getHeaders().put(name, value);
        return this;
    }
}
