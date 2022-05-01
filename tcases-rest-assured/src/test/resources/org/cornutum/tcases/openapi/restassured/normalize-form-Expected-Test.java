package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class NormalizeFormTest {

    @Test
    public void getCookieArray_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "")
            .cookie( "nonEmpty", "-693275569")
            .cookie( "nonEmpty", "234977157")
            .cookie( "nonEmpty", "876471102")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_EmptyItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", null)
            .cookie( "exploded", "-419836067")
            .cookie( "nonEmpty", null)
            .cookie( "nonEmpty", "373530702")
            .cookie( "nonEmpty", null)
            .cookie( "nonEmpty", "426044715")
            .cookie( "empty", "-711113154")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_EmptyItemsSize_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "-868010130")
            .cookie( "exploded", null)
            .cookie( "exploded", "-200255833")
            .cookie( "exploded", "-474460415")
            .cookie( "nonEmpty", "0")
            .cookie( "nonEmpty", "-93610387")
            .cookie( "nonEmpty", "-314004569")
            .cookie( "empty", null)
            .cookie( "empty", "378798126")
            .cookie( "empty", "-159738185")
            .cookie( "empty", "299511675")
            .cookie( "empty", "289949802")
            .cookie( "empty", "-166763582")
            .cookie( "empty", "-732952567")
            .cookie( "empty", "-434911013")
            .cookie( "empty", "-885448460")
            .cookie( "empty", "294411106")
            .cookie( "empty", "-173016726")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_EmptyItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", null)
            .cookie( "nullable", "-547326678")
            .cookie( "nullable", "-201649904")
            .cookie( "nullable", "-236934060")
            .cookie( "nullable", "-664180684")
            .cookie( "nullable", "-542591555")
            .cookie( "nullable", "-311854710")
            .cookie( "nullable", "232388876")
            .cookie( "nullable", "-871691332")
            .cookie( "nullable", "27461401")
            .cookie( "exploded", "")
            .cookie( "nonEmpty", "113565203")
            .cookie( "nonEmpty", "928807526")
            .cookie( "nonEmpty", "-617379025")
            .cookie( "nonEmpty", "6628590")
            .cookie( "nonEmpty", "682405934")
            .cookie( "nonEmpty", "682405934")
            .cookie( "nonEmpty", "884557134")
            .cookie( "empty", "0")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_EmptyItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "0")
            .cookie( "nonEmpty", "-214932494")
            .cookie( "nonEmpty", "69016107")
            .cookie( "nonEmpty", "299796916")
            .cookie( "empty", "649236696")
            .cookie( "empty", "405376511")
            .cookie( "empty", "649236696")
            .cookie( "empty", "420067520")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_NullableItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "0")
            .cookie( "exploded", "1038578431")
            .cookie( "exploded", "-784181049")
            .cookie( "exploded", "-606961340")
            .cookie( "exploded", "-621002828")
            .cookie( "exploded", "431150463")
            .cookie( "exploded", "370668224")
            .cookie( "exploded", "-902248274")
            .cookie( "exploded", "-1029138552")
            .cookie( "exploded", "792473812")
            .cookie( "exploded", "353059041")
            .cookie( "exploded", "-303763829")
            .cookie( "exploded", "203698345")
            .cookie( "exploded", "-621002828")
            .cookie( "exploded", "509547361")
            .cookie( "exploded", "579344301")
            .cookie( "nonEmpty", "0")
            .cookie( "nonEmpty", "250512574")
            .cookie( "nonEmpty", "1011382367")
            .cookie( "nonEmpty", "0")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_NullableItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "1069935703")
            .cookie( "nullable", "312911395")
            .cookie( "nullable", "-472955057")
            .cookie( "nullable", "46871944")
            .cookie( "nullable", "922474989")
            .cookie( "nullable", "-375962479")
            .cookie( "nullable", "312911395")
            .cookie( "exploded", "")
            .cookie( "nonEmpty", "1027508012")
            .cookie( "nonEmpty", "276167015")
            .cookie( "nonEmpty", "-602979065")
            .cookie( "empty", "-310258782")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieArray_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-520920333")
            .cookie( "nonEmpty", "-280293421")
            .cookie( "nonEmpty", "828455590")
            .cookie( "nonEmpty", "896281385")
            .cookie( "nonEmpty", "-22111446")
            .cookie( "nonEmpty", "-890563021")
            .cookie( "nonEmpty", "498089792")
            .cookie( "nonEmpty", "-492936661")
            .cookie( "nonEmpty", "17892237")
            .cookie( "nonEmpty", "-878917158")
            .cookie( "nonEmpty", "896281385")
            .cookie( "nonEmpty", "-677178337")
            .cookie( "nonEmpty", "415977208")
            .cookie( "nonEmpty", "-410401003")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-219360516")
            .cookie( "nonEmpty", "-1067585109")
            .cookie( "nonEmpty", "-312664329")
            .cookie( "nonEmpty", "571254439")
            .cookie( "nonEmpty", "-264533851")
            .cookie( "nonEmpty", "-578549637")
            .cookie( "nonEmpty", "-65420259")
            .cookie( "nonEmpty", "-265151844")
            .cookie( "nonEmpty", "-640769611")
            .cookie( "nonEmpty", "-264533851")
            .cookie( "nonEmpty", "887380090")
            .cookie( "nonEmpty", "-833726260")
            .cookie( "nonEmpty", "534137616")
            .cookie( "nonEmpty", "51594259")
            .cookie( "nonEmpty", "-22725447")
            .cookie( "nonEmpty", "-946993709")
            .cookie( "nonEmpty", "-923690769")
            .cookie( "empty", null)
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_EmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-971153934")
            .cookie( "nonEmpty", "-46136310")
            .cookie( "nonEmpty", "601790066")
            .cookie( "nonEmpty", "468117411")
            .cookie( "nonEmpty", "468117411")
            .cookie( "empty", "z*$:")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // empty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_EmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-470140281")
            .cookie( "nonEmpty", "-348384047")
            .cookie( "nonEmpty", "-918016421")
            .cookie( "nonEmpty", "-978975016")
            .cookie( "nonEmpty", "219095384")
            .cookie( "nonEmpty", "916856980")
            .cookie( "nonEmpty", "706404414")
            .cookie( "nonEmpty", "475986893")
            .cookie( "nonEmpty", "-978975016")
            .cookie( "nonEmpty", "-1066747705")
            .cookie( "nonEmpty", "1039176339")
            .cookie( "nonEmpty", "706073155")
            .cookie( "nonEmpty", "-242829193")
            .cookie( "empty", "lgonfpdrbju,-645,wheojceio,true,ctfoeqbf,true")
            .cookie( "empty", "964079817")
            .cookie( "empty", "566103149")
            .cookie( "empty", "797478716")
            .cookie( "empty", "1011477758")
            .cookie( "empty", "446377302")
            .cookie( "empty", "-245330304")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // empty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-961480436")
            .cookie( "empty", "0")
            .cookie( "empty", "-1063628728")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-784756820")
            .cookie( "nonEmpty", null)
            .cookie( "empty", "0")
            .cookie( "empty", "850414313")
            .cookie( "empty", "-980247388")
            .cookie( "empty", "-975252053")
            .cookie( "empty", "785301097")
            .cookie( "empty", "-1039494315")
            .cookie( "empty", "-350788845")
            .cookie( "empty", "-144863154")
            .cookie( "empty", "852118866")
            .cookie( "empty", "-683081263")
            .cookie( "empty", "-468753639")
            .cookie( "empty", "535816119")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NonEmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-295796373")
            .cookie( "nonEmpty", "5`")
            .cookie( "empty", "0")
            .cookie( "empty", "-778348244")
            .cookie( "empty", "276152649")
            .cookie( "empty", "585410331")
            .cookie( "empty", "-379919888")
            .cookie( "empty", "-415285518")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nonEmpty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NonEmptyItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-1046855985")
            .cookie( "nonEmpty", "-957438530")
            .cookie( "nonEmpty", "-957438530")
            .cookie( "empty", "0")
            .cookie( "empty", "750113639")
            .cookie( "empty", "76439899")
            .cookie( "empty", "-203533297")
            .cookie( "empty", "-476767616")
            .cookie( "empty", "-55273538")
            .cookie( "empty", "-384666176")
            .cookie( "empty", "-450164729")
            .cookie( "empty", "793958981")
            .cookie( "empty", "-1005618586")
            .cookie( "empty", "-91059929")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nonEmpty.Items.Size=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NonEmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-438254667")
            .cookie( "nonEmpty", "")
            .cookie( "nonEmpty", "639846759")
            .cookie( "nonEmpty", "874174903")
            .cookie( "nonEmpty", "-127178769")
            .cookie( "nonEmpty", "-640682667")
            .cookie( "nonEmpty", "317729306")
            .cookie( "nonEmpty", "-896194266")
            .cookie( "nonEmpty", "-348776846")
            .cookie( "nonEmpty", "639846759")
            .cookie( "nonEmpty", "-834941380")
            .cookie( "nonEmpty", "-151280218")
            .cookie( "nonEmpty", "178048144")
            .cookie( "nonEmpty", "496343853")
            .cookie( "nonEmpty", "511117452")
            .cookie( "empty", "0")
            .cookie( "empty", "885503261")
            .cookie( "empty", "-410957425")
            .cookie( "empty", "-526182998")
            .cookie( "empty", "21299556")
            .cookie( "empty", "472188538")
            .cookie( "empty", "-784988105")
            .cookie( "empty", "-637711743")
            .cookie( "empty", "407027609")
            .cookie( "empty", "716194532")
            .cookie( "empty", "559769349")
            .cookie( "empty", "-581831249")
            .cookie( "empty", "-448314537")
            .cookie( "empty", "741135287")
            .cookie( "empty", "-477878491")
            .cookie( "empty", "-46029641")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nonEmpty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "exploded", "-107530337")
            .cookie( "nonEmpty", "-518643973")
            .cookie( "nonEmpty", "-77974426")
            .cookie( "nonEmpty", "937901134")
            .cookie( "nonEmpty", "-77974426")
            .cookie( "nonEmpty", "-680038370")
            .cookie( "nonEmpty", "-976656316")
            .cookie( "nonEmpty", "-963719753")
            .cookie( "empty", "0")
            .cookie( "empty", "-91366725")
            .cookie( "empty", "283003595")
            .cookie( "empty", "-883416592")
            .cookie( "empty", "-531206433")
            .cookie( "empty", "450712482")
            .cookie( "empty", "871808811")
            .cookie( "empty", "703475292")
            .cookie( "empty", "-754077418")
            .cookie( "empty", "779983738")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NullableType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "-1045861582")
            .cookie( "nonEmpty", "-454829851")
            .cookie( "nonEmpty", "-984930390")
            .cookie( "nonEmpty", "-700830862")
            .cookie( "nonEmpty", "913564250")
            .cookie( "nonEmpty", "889594236")
            .cookie( "nonEmpty", "-525405188")
            .cookie( "nonEmpty", "404051226")
            .cookie( "nonEmpty", "31982686")
            .cookie( "nonEmpty", "913564250")
            .cookie( "empty", "0")
            .cookie( "empty", "231816045")
            .cookie( "empty", "530223669")
            .cookie( "empty", "-453239323")
            .cookie( "empty", "146591454")
            .cookie( "empty", "767942963")
            .cookie( "empty", "331560179")
            .cookie( "empty", "-64770506")
            .cookie( "empty", "-606510930")
            .cookie( "empty", "855466399")
            .cookie( "empty", "825014212")
            .cookie( "empty", "118860946")
            .cookie( "empty", "-69872")
            .cookie( "empty", "1049859350")
            .cookie( "empty", "594364588")
            .cookie( "empty", "-164982354")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nullable.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_NullableItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "zmlasyqdcuf,1oV()V(,P(Uy)ypn,0]f4X,utz,/UF8,2%!8f5P:")
            .cookie( "exploded", "-212337829")
            .cookie( "nonEmpty", "-191775723")
            .cookie( "nonEmpty", "100042277")
            .cookie( "nonEmpty", "-740070282")
            .cookie( "nonEmpty", "-780291820")
            .cookie( "nonEmpty", "-929418765")
            .cookie( "nonEmpty", "-154357005")
            .cookie( "nonEmpty", "-740151940")
            .cookie( "nonEmpty", "936810927")
            .cookie( "nonEmpty", "-1000456551")
            .cookie( "nonEmpty", "635696053")
            .cookie( "nonEmpty", "998513341")
            .cookie( "nonEmpty", "643943299")
            .cookie( "nonEmpty", "-419879738")
            .cookie( "nonEmpty", "-927960791")
            .cookie( "nonEmpty", "643943299")
            .cookie( "empty", "0")
            .cookie( "empty", "-319167594")
            .cookie( "empty", "583713219")
            .cookie( "empty", "-476159324")
            .cookie( "empty", "-883461249")
            .cookie( "empty", "786226251")
            .cookie( "empty", "-4426874")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // nullable.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "nonEmpty", "-536921145")
            .cookie( "nonEmpty", "423262072")
            .cookie( "nonEmpty", "427978470")
            .cookie( "nonEmpty", "-803605758")
            .cookie( "nonEmpty", "50939491")
            .cookie( "nonEmpty", "-128893322")
            .cookie( "nonEmpty", "-839697190")
            .cookie( "nonEmpty", "-964174352")
            .cookie( "nonEmpty", "-128893322")
            .cookie( "nonEmpty", "481351853")
            .cookie( "nonEmpty", "-815714201")
            .cookie( "nonEmpty", "-49504136")
            .cookie( "nonEmpty", "1020860350")
            .cookie( "nonEmpty", "-765260662")
            .cookie( "nonEmpty", "-427525978")
            .cookie( "empty", "0")
            .cookie( "empty", "-277870227")
            .cookie( "empty", "391936768")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", null)
            .cookie( "nonEmpty", "-113146619")
            .cookie( "nonEmpty", "191112349")
            .cookie( "nonEmpty", "169394460")
            .cookie( "nonEmpty", "-367913744")
            .cookie( "nonEmpty", "-961782069")
            .cookie( "nonEmpty", "259255816")
            .cookie( "nonEmpty", "471818804")
            .cookie( "nonEmpty", "-904872868")
            .cookie( "nonEmpty", "-438382359")
            .cookie( "nonEmpty", "27268092")
            .cookie( "nonEmpty", "-438382359")
            .cookie( "nonEmpty", "197457732")
            .cookie( "nonEmpty", "-672271456")
            .cookie( "nonEmpty", "-371247503")
            .cookie( "nonEmpty", "821335955")
            .cookie( "nonEmpty", "222149137")
            .cookie( "empty", "0")
            .cookie( "empty", "441013913")
            .cookie( "empty", "-751473119")
            .cookie( "empty", "885406283")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_ExplodedType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "726")
            .cookie( "nonEmpty", "-909948647")
            .cookie( "nonEmpty", "-916741101")
            .cookie( "nonEmpty", "-738550284")
            .cookie( "nonEmpty", "88684308")
            .cookie( "nonEmpty", "-200010778")
            .cookie( "nonEmpty", "-569605712")
            .cookie( "nonEmpty", "658135834")
            .cookie( "nonEmpty", "-995148438")
            .cookie( "nonEmpty", "131156545")
            .cookie( "nonEmpty", "-437623294")
            .cookie( "nonEmpty", "-829524555")
            .cookie( "nonEmpty", "-188912641")
            .cookie( "nonEmpty", "-807629960")
            .cookie( "nonEmpty", "88684308")
            .cookie( "nonEmpty", "-452321412")
            .cookie( "nonEmpty", "98633082")
            .cookie( "empty", "0")
            .cookie( "empty", "-880121896")
            .cookie( "empty", "-699913628")
            .cookie( "empty", "-40331156")
            .cookie( "empty", "-178555385")
            .cookie( "empty", "-443214733")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // exploded.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieArray_ExplodedItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "exploded", "")
            .cookie( "nonEmpty", "-179064107")
            .cookie( "nonEmpty", "590837365")
            .cookie( "nonEmpty", "67926625")
            .cookie( "nonEmpty", "120403315")
            .cookie( "nonEmpty", "847261358")
            .cookie( "nonEmpty", "-822871557")
            .cookie( "nonEmpty", "273120405")
            .cookie( "nonEmpty", "850316692")
            .cookie( "nonEmpty", "232948448")
            .cookie( "nonEmpty", "273120405")
            .cookie( "empty", "0")
            .cookie( "empty", "676134817")
            .cookie( "empty", "346922282")
            .cookie( "empty", "461384042")
            .cookie( "empty", "-326297035")
            .cookie( "empty", "-82274617")
            .cookie( "empty", "-174575129")
            .cookie( "empty", "785730227")
            .cookie( "empty", "215614091")
        .when()
            .request( "GET", "/cookie/array")
        .then()
            // exploded.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", "0")
            .cookie( "deep[height]", "0")
            .cookie( "deep[rvwzdnvop]", "true")
            .cookie( "deep[nnkvbeketviri]", "gtemhythxoklifau,-820")
            .cookie( "nullable", null)
            .cookie( "width", "0")
            .cookie( "height", "0")
            .cookie( "jq", "-405.6")
            .cookie( "fs", "pa,sCvVE")
            .cookie( "zupzuvae", "true")
            .cookie( "width", "0")
            .cookie( "height", "0")
            .cookie( "vrvjtzqohfam", "xqkftpd,W/mbc")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", null)
            .cookie( "nullable", "width|0|height|0|wmcosailavwu|ppmqtgj,2E5,xhk,935.4|ztg|375.3|sxiso|true")
            .cookie( "width", null)
            .cookie( "width", null)
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", null)
            .cookie( "deep[xlvmsgsio]", "-506")
            .cookie( "deep[eph]", "-931")
            .cookie( "deep[vojqnqnhga]", "lztclbqxxgpayxo,sKK~c~RH,qsxgtjthriogfuvj,c>,|!.UBXs,igfkyvej,P7^R1:u,WL-G^X")
            .cookie( "nullable", "width|")
            .cookie( "height", null)
            .cookie( "wkpywkvfcihwhdr", "-rwwE")
            .cookie( "ajkbvhoedfuwnctv", "-898")
            .cookie( "width", "973412376")
            .cookie( "height", null)
            .cookie( "hvvvjrjriswmmbu", "282")
            .cookie( "niawgtgguskd", "-76.9")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", "476975980")
            .cookie( "nullable", "height||jqruj|luI`|n||obqx|-223.3")
            .cookie( "width", "497293935")
            .cookie( "height", "58029920")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "83227616")
            .cookie( "deep[chtojvpmlmbmgz]", "125.7")
            .cookie( "nullable", "width|50874597")
            .cookie( "height", "12246800")
            .cookie( "owvcwpddtwmxbs", "true")
            .cookie( "hyipxux", "true")
            .cookie( "vtqy", "-154.7")
            .cookie( "width", "0")
            .cookie( "pwcgljizibbhaw", "714.1")
            .cookie( "ncenqharcbfatnao", "=U@r,&*T/*,#X]O1.]w")
            .cookie( "mkbyskundofwu", "O")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", "0")
            .cookie( "nullable", "height|144325543|ehr|wxknlugzcxldftok,-588.2,xssjhoezflfzlm,true,wxskhytnwgmk,true|ptifryawqwqvqy|8-b")
            .cookie( "width", "0")
            .cookie( "height", "0")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[kt]", "293.5")
            .cookie( "deep[qpgfx]", "NZt?n,mfm#.B~7")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "idlbskufmk", "lhytl,846,cf,oc,syglecxcfux,1002.7")
            .cookie( "sacwjozkf", "++Lui,.!AT7_3,")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[xtvldchkt]", "true")
            .cookie( "deep[zuhw]", "switmyjo,~Rh")
            .cookie( "deep[qac]", "R")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "prq", "hoauxgugp,")
            .cookie( "nonEmpty", null)
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[oykzuthmftzixfua]", "m5H")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "zcvsaku", "rusrpqqyjruqzf,true,mypwlwbfdrh,true")
            .cookie( "nonEmpty", ")qoFS")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[knl]", "-143")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "ervitocbuawplri", "-76.5")
            .cookie( "phdmvzi", "true")
            .cookie( "unypzkdqjoqxep", "-444")
            .cookie( "width", "309.2")
            .cookie( "vwioaxdqwsnr", "uip,true,yhhmqiplskvbpp,-947")
            .cookie( "spcjdi", "Od09c~")
            .cookie( "pjfslhyfqgto", ")_c")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[wdrfuxdmorpiwd]", "GqMgT")
            .cookie( "deep[zkbk]", "-866")
            .cookie( "deep[nxysnlvin]", "-388")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "ogykxrg", "-160")
            .cookie( "width", "-1")
            .cookie( "ldauvvfvnf", "843.7")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[allfyf]", "81.8")
            .cookie( "deep[cewsqkk]", "606.4")
            .cookie( "deep[uqqlluaysu]", "")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "ccwzzsfe", "true")
            .cookie( "lcjsmnnrcosx", "true")
            .cookie( "hesvrs", "403")
            .cookie( "width", "708800584")
            .cookie( "height", "213.0")
            .cookie( "bzsyzslyojdn", "lk?OR!T")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[mmzamtkj]", "+=K/[z")
            .cookie( "deep[xuthegtgnbph]", "eyepnibfxd,i)X!,wogfxxvsepyxxobv,true,gqwxee,")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "vbtwizqybaqejw", "true")
            .cookie( "anejezntzyyrkkte", "")
            .cookie( "chkvcdbhfszxvfzl", "-90")
            .cookie( "width", "448646942")
            .cookie( "height", "-1")
            .cookie( "mqobeqnsg", "6")
            .cookie( "lguzjfpwgzhztrsl", "-926.2")
            .cookie( "dqeflxjdhd", "yF`aS")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nonEmpty.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[n]", "505.1")
            .cookie( "deep[uhwh]", "579")
            .cookie( "deep[zpevx]", "-758.2")
            .cookie( "height", "0")
            .cookie( "bexsjhaxpylaaqxc", "abmfafywtkimg,802,hvxrmiuluclksk,,sixgnfavmefmyc,u<x{pxUH,Xpt,Q`[lFG")
            .cookie( "width", "476917625")
            .cookie( "rmswz", "-539.9")
            .cookie( "zxgqouwpunxr", "-991")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[vgkrv]", "241")
            .cookie( "deep[dnscgq]", "13")
            .cookie( "deep[xmsjasvroztkgpli]", "")
            .cookie( "nullable", "517")
            .cookie( "height", "0")
            .cookie( "sroecrzmimky", "IKzA3J,@VO")
            .cookie( "megg", "true")
            .cookie( "socgftdel", "true")
            .cookie( "width", "604202637")
            .cookie( "nwg", "true")
            .cookie( "ook", "328.5")
            .cookie( "ezk", "")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[ja]", "295")
            .cookie( "nullable", "width|true")
            .cookie( "height", "0")
            .cookie( "nbuefvwtotwmku", "true")
            .cookie( "width", "11613031")
            .cookie( "ieezkmgevhuf", "],APJG:")
            .cookie( "adgxnsg", "true")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[asbqifhhltdg]", "My5Z_vgz")
            .cookie( "nullable", "width|-1")
            .cookie( "height", "0")
            .cookie( "f", "")
            .cookie( "kxlfnwd", "539")
            .cookie( "width", "881389499")
            .cookie( "nmvdzial", "c`nvf")
            .cookie( "ijdfmw", "VNzd=e")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[kcpgdmtubl]", "p1/,m")
            .cookie( "deep[la]", "")
            .cookie( "deep[ycgfpepvpkoyfcmz]", "-239")
            .cookie( "nullable", "width|0|height|n!KAC")
            .cookie( "height", "0")
            .cookie( "teollfur", "true")
            .cookie( "arlzcfetyvklqhfi", "")
            .cookie( "xdbuqg", "dpriupeuslp,true,nx,-62.7")
            .cookie( "width", "353796654")
            .cookie( "wtwr", "kB,'#|aR")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[vzlipfjgqhtqy]", "lxgizr,-137.4")
            .cookie( "deep[fz]", "273")
            .cookie( "deep[psvatiilhbeel]", "-678.9")
            .cookie( "nullable", "width|0|height|-1")
            .cookie( "height", "0")
            .cookie( "gvoogburzu", "j*]NUiQz")
            .cookie( "ym", "clex,o~,.Y@,qogftmahijmzp,-873.2")
            .cookie( "width", "624567118")
            .cookie( "kg", "wB+(VSr")
            .cookie( "hytdemnxeh", "~#-")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // nullable.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[jmhzanuczknxzlyk]", "M}")
            .cookie( "deep[eu]", "true")
            .cookie( "deep[hjlsbusaicygx]", "Np")
            .cookie( "nullable", "width|0")
            .cookie( "width", "983646528")
            .cookie( "v", "-361")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[q]", "'B(u3,ai[V)1<n,|cq")
            .cookie( "deep[jwova]", "CAQHZ|1-,c1,8H")
            .cookie( "deep[uoxrmsfwbrxts]", "true")
            .cookie( "nullable", "width|0")
            .cookie( "exploded", null)
            .cookie( "width", "543156382")
            .cookie( "auydpvv", "765.7")
            .cookie( "isfghciepiwkj", "xhkpkbcb,true")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[y]", "-825.1")
            .cookie( "deep[sxjkshovwz]", "-793.1")
            .cookie( "deep[vjf]", "-477.0")
            .cookie( "nullable", "width|0")
            .cookie( "exploded", "-460.3")
            .cookie( "width", "598172130")
            .cookie( "pfmrh", "Fc,yPS:W>R@,/$>wQ^")
            .cookie( "bsyvozhahkcfckr", "srgftdslr,693")
            .cookie( "illrby", "uta,-508,xfx,true")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[wvozdcudal]", "gLG")
            .cookie( "deep[zuliplqfhtx]", "true")
            .cookie( "deep[wnqupqdhyoilvlbq]", "mkytwrfarmqmgx,true,xlyidypivznlif,-615.7")
            .cookie( "nullable", "width|0")
            .cookie( "width", "true")
            .cookie( "height", "0")
            .cookie( "hjgesb", "jjlx,P9J")
            .cookie( "nzvt", "-598.2")
            .cookie( "qlguiodj", "")
            .cookie( "width", "236545287")
            .cookie( "miuqbvbnyljnx", "-762")
            .cookie( "o", "-429")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[icd]", ",3?-ic,RV.@")
            .cookie( "deep[iadtrwupwybuf]", "jvifpgtvnoufrcl,true")
            .cookie( "nullable", "width|0")
            .cookie( "width", "-1")
            .cookie( "height", "0")
            .cookie( "zjqscmj", "true")
            .cookie( "width", "830812068")
            .cookie( "nkmvqxrdjgtf", "?Hl<%_/")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[icxbvooupr]", "true")
            .cookie( "nullable", "width|0")
            .cookie( "height", "bjxo,-3")
            .cookie( "xulmdrulzxkylqa", "qzzqmneb,true,ygfdgawycgw,1t")
            .cookie( "width", "1049916967")
            .cookie( "oxtbnivr", "")
            .cookie( "hbv", "true")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "0")
            .cookie( "deep[tcmnjymupmm]", "947")
            .cookie( "deep[kqapkfsel]", "1018.0")
            .cookie( "nullable", "width|0")
            .cookie( "height", "-1")
            .cookie( "jthfeimgqj", "-80.4")
            .cookie( "hchj", "{")
            .cookie( "width", "842975891")
            .cookie( "nho", "gwpwvdaeinbo,^^`*,u3C$,flredjl,CrSNzb,rnmuqhpvkdnnmzsf,530")
            .cookie( "lksm", "tCx+S)v")
            .cookie( "yd", "690")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // exploded.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "baookrgkpa", "true")
            .cookie( "width", "69041269")
            .cookie( "dmmpydaap", "<")
            .cookie( "hd", "-378.0")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep", null)
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "lbhl", "t1z!o")
            .cookie( "jrbrzdiqeogno", "true")
            .cookie( "width", "269429297")
            .cookie( "nteolbekop", "-1001.6")
            .cookie( "bta", "true")
            .cookie( "rhcszzagu", "-752")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep", "32")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "zfhsrvfwqlwqjx", "znpdnttnjvxucdsp,-132,uykyuzpytb,827.4,dwwvvzoohqbzlpuk,aYc*B8jM")
            .cookie( "fbzfzov", "547.9")
            .cookie( "cmzmtvntx", "pyenc,=o9r.S,hktdhlp,r7$")
            .cookie( "width", "509069980")
            .cookie( "t", "r89:{A&:,SLs")
            .cookie( "vnqycgxjgnlllboh", "-161")
            .cookie( "pr", "7u]?lf,.|}eta")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", "")
            .cookie( "deep[height]", "0")
            .cookie( "deep[cuxnauccukcxucyd]", ":}")
            .cookie( "deep[cbvtpmxrgib]", "eeucziiknyex,-891.5,chklbagbipbouq,t")
            .cookie( "deep[btihrkynjjqpod]", "-666")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "fcitbbmeeo", "418.6")
            .cookie( "width", "949340163")
            .cookie( "onycjodx", "h,w3N,bnac,true")
            .cookie( "yjfpfqjsov", "249.6")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[width]", "-1")
            .cookie( "deep[height]", "0")
            .cookie( "deep[ulkkyjsdysbnul]", "tRT")
            .cookie( "deep[bpykwnexzvilq]", "true")
            .cookie( "deep[rffbvhbdrsmzf]", "81")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "nkrquccepk", "rjtrnsul,L|#h02),slby,")
            .cookie( "mamnhulptfuiqzg", "-639")
            .cookie( "pcsc", "true")
            .cookie( "width", "1031437721")
            .cookie( "smykgxtgj", "")
            .cookie( "rc", "-7z-e@")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", ")`8%v3")
            .cookie( "deep[mtocpufz]", "pbfmprnigjjp,461,caannwoanhe,0|4_GP,sexdwaeixjt,316")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "skfgz", "-865.0")
            .cookie( "rgbfgtlhbcdycw", "yiiyrmsa,true,sdyxuph,723.6,hliqdozpo,")
            .cookie( "width", "688690749")
            .cookie( "afjindgh", "")
            .cookie( "zkmaqa", "996")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieObject_DeepValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .cookie( "deep[height]", "-1")
            .cookie( "deep[kztulkvgosvrvh]", "-331.5")
            .cookie( "deep[inzzdziwmceakja]", "")
            .cookie( "deep[uttaomjplr]", "X-Pf#NEJ")
            .cookie( "nullable", "width|0")
            .cookie( "height", "0")
            .cookie( "yesoilhmw", "PE'_Q~X")
            .cookie( "waknahxuuawfks", "tvbenftoxmqdf,NMnz,eseynvlornzsq,>-H")
            .cookie( "hqlbrgbgohofe", "true")
            .cookie( "width", "1025897736")
            .cookie( "pwaplgtetqe", ",h!0e^_o5")
            .cookie( "g", "409")
            .cookie( "jyovnmqlmql", "{RIhBrJe")
        .when()
            .request( "GET", "/cookie/object")
        .then()
            // deep.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "UqxI`or3HK*Gr&")
            .cookie( "nonEmpty", "r0w")
            .cookie( "empty", "+x2|c4~|H7qB}4bA|3.jNsV{AHZb#_IL-Lk>s`$JPf}9+g4z&a([AJt]Rv3A8>5Xb(%`#f8JQfr}s'#z@#P:|fP?#_[871k$aw'7pfRE>kykb4%4A6IE25'qi~!$RtH4Y@~")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieString_EmptyValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", null)
            .cookie( "nonEmpty", ".Zb@=J^%uod#A:70pu&OXw&1i$)F9n{5q_NX|I?|yUwY6vxsvDZV46N0b)[EBGr'2hthz?n~LIB}puz?M(2$3}lo?szZyyuqFJ46z#D9xl^r(eA^z)P!?8C(j7ITRYVnbvH&MHUX_F8im}{18%0F{qvd9if^3o<Lw{mr2&.:C|ur?-b!UnL{!&9gS+9vXkQOFe?8=ZJ<kz[0yvPD=r)YOK(B1f*k7Xe0Y1C")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieString_NullableValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "")
            .cookie( "nonEmpty", "q(h")
            .cookie( "empty", "s9`_f|ymd0s4:'Nm#hr3J0__+X=#/v^CzVV<h/?]d0aeQm=yEU1Kz<6j9:@|N&v[FW#y4YT/3`18vnf)nl'g|~e9JKGUpKK7LNzbKrt?&We@K57|DL55xw~X|lt6E$r(~v.e<&i0vbSnyj?1|bN2!MLM(`{p56W12:Hvz7r")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getCookieString_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "jN@KE8~*&FC9^*J2]3Wsc^nUOdkKVT(tdYu_{>/-tik.bVJj7=.eT!")
            .cookie( "nonEmpty", "B7O9m|qd8ic!kdZHc|B3mKKaO]I$t^M|k$P_{./]m$(fxI(4A]XrKb[JFL*KGK(Wb5||1U3J3U#qM_a~aG?)xbW-vK|JZ@b)!nw8pO=gVO-{{VwYehwMZ([Of98piGgp0:@K[v-/Hs7aFHAR'X.pxGf09:uC|9MOP__wv4HQrZ/o/Wf9abj0S<5OSZl`qvvApG!#js0")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "*9$+$vZG@Adi^=-E<_6RE#IjoA`|L:fEr4'Q&/7U-D){onn1[FRtF2U*7]1M(_/bY^1Eo`!l`$5*@${wRj)v_tjVI|voo4~<QqQp!oF[1&[V$ah6eM|zKfnXqJ48QK4cJFC7fnV5x0QIgB720J]TrPCHct%8~-j=fQ:mS(2eb6{:iC/LQ)2'Z^$vWBuG-$bEu6seP8p$cVof*AAR")
            .cookie( "nonEmpty", "80IR{Jv!WcBW[=L5:Zc{NuP(Gbg[JQ[Xw<.'#}B>w|>up#-nb^bI[?")
            .cookie( "empty", null)
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "i:CfWyF.k{0u0#do:4LO=_J=cPUURb-e8@a(r_g&%vARlq^ab.Mxf1=$`UZ={_R#fUhZ{y}<SL`h<UbdBNgm(2f(S:ODeK^}V&dz/asxNzc!<(yTi1eX@")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "HmL2RP(QB}(e*?DT6{P>(3I#u|d%*0R")
            .cookie( "nonEmpty", null)
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_NonEmptyValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .cookie( "nullable", "g/l_/b^0Ho7c1ICX%i@kZAb-4O!WMRR^lV:7)TH+ISlE<P2YnpN5OF7?}?s9Qj}Gq&6n@39(b<otAk`gk-f*c2Jg%zlS+RC)JNL@~uKe^YtI)}:6U_xYCv3=vJr*bj.0pFU#a)-F>pC_Ns+axROFD}-|}_4~{z!ze4yIG8wr:g")
            .cookie( "nonEmpty", "0#")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // nonEmpty.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getCookieString_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "nonEmpty", "<zjM}v~=VBFA)cM&=DA|AW8y/4npnzl*dUaMY>6r0n.0_)!7vd[wd_5FrOq/]{4a2}/u3h>F$+{K9/.z.HbOek2po*@)J|$<Vu/NW&j^T^OT0c92Pw#mB|.#236M&sp]4^3zo.g0)A5<c9ZBWzi/w?6L<$`~y5jlt}jkua7pS80")
            .cookie( "empty", "")
        .when()
            .request( "GET", "/cookie/string")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "")
            .queryParam( "nonEmpty", "-946758145")
            .queryParam( "nonEmpty", "976538274")
            .queryParam( "nonEmpty", "-1049779932")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_EmptyItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", (String) null)
            .queryParam( "exploded", "-409788165")
            .queryParam( "nonEmpty", (String) null)
            .queryParam( "nonEmpty", "244527022")
            .queryParam( "nonEmpty", "541605225")
            .queryParam( "nonEmpty", "-818255407")
            .queryParam( "nonEmpty", "-193544758")
            .queryParam( "nonEmpty", "-319090172")
            .queryParam( "nonEmpty", "483659607")
            .queryParam( "nonEmpty", "-173886350")
            .queryParam( "nonEmpty", "-432759842")
            .queryParam( "nonEmpty", "-840160859")
            .queryParam( "nonEmpty", "-276680525")
            .queryParam( "nonEmpty", "541605225")
            .queryParam( "nonEmpty", "-937206653")
            .queryParam( "nonEmpty", "-832672209")
            .queryParam( "nonEmpty", "630278341")
            .queryParam( "empty", "-568615503")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_EmptyItemsSize_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "-236675136")
            .queryParam( "exploded", (String) null)
            .queryParam( "exploded", "1004225667")
            .queryParam( "exploded", "-690938211")
            .queryParam( "exploded", "-138887481")
            .queryParam( "exploded", "-911746819")
            .queryParam( "exploded", "295321005")
            .queryParam( "nonEmpty", "0")
            .queryParam( "nonEmpty", "-89076005")
            .queryParam( "nonEmpty", "800966307")
            .queryParam( "empty", (String) null)
            .queryParam( "empty", "-461638436")
            .queryParam( "empty", "-15830534")
            .queryParam( "empty", "-801618176")
            .queryParam( "empty", "-542755596")
            .queryParam( "empty", "930079291")
            .queryParam( "empty", "-615477119")
            .queryParam( "empty", "231473247")
            .queryParam( "empty", "-747779497")
            .queryParam( "empty", "-1043022183")
            .queryParam( "empty", "-370754972")
            .queryParam( "empty", "-1054827875")
            .queryParam( "empty", "123553467")
            .queryParam( "empty", "183027205")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_EmptyItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", (String) null)
            .queryParam( "nullable", "-867763683")
            .queryParam( "nullable", "891562829")
            .queryParam( "nullable", "-311604734")
            .queryParam( "nullable", "25004459")
            .queryParam( "nullable", "440827284")
            .queryParam( "nullable", "643562827")
            .queryParam( "nullable", "549383527")
            .queryParam( "exploded", "")
            .queryParam( "nonEmpty", "103026289")
            .queryParam( "nonEmpty", "-117342421")
            .queryParam( "nonEmpty", "-249592874")
            .queryParam( "nonEmpty", "-654625390")
            .queryParam( "nonEmpty", "-374764058")
            .queryParam( "nonEmpty", "-947054118")
            .queryParam( "nonEmpty", "92548822")
            .queryParam( "nonEmpty", "-147471349")
            .queryParam( "nonEmpty", "-36517737")
            .queryParam( "nonEmpty", "653875233")
            .queryParam( "nonEmpty", "361362064")
            .queryParam( "nonEmpty", "-489511924")
            .queryParam( "nonEmpty", "397261534")
            .queryParam( "nonEmpty", "-489511924")
            .queryParam( "empty", "0")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_EmptyItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "0")
            .queryParam( "nonEmpty", "-816934983")
            .queryParam( "nonEmpty", "-153606395")
            .queryParam( "nonEmpty", "335508388")
            .queryParam( "empty", "936666193")
            .queryParam( "empty", "-571813211")
            .queryParam( "empty", "128242613")
            .queryParam( "empty", "-781505315")
            .queryParam( "empty", "-570263001")
            .queryParam( "empty", "-921719413")
            .queryParam( "empty", "121200832")
            .queryParam( "empty", "715649796")
            .queryParam( "empty", "792863197")
            .queryParam( "empty", "-781505315")
            .queryParam( "empty", "882457041")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_NullableItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "0")
            .queryParam( "exploded", "967908623")
            .queryParam( "exploded", "-377188700")
            .queryParam( "exploded", "-822201812")
            .queryParam( "exploded", "-994796150")
            .queryParam( "exploded", "78846419")
            .queryParam( "exploded", "-753444122")
            .queryParam( "exploded", "-880047740")
            .queryParam( "exploded", "-567647683")
            .queryParam( "exploded", "-924716132")
            .queryParam( "exploded", "-924716132")
            .queryParam( "nonEmpty", "0")
            .queryParam( "nonEmpty", "368833137")
            .queryParam( "nonEmpty", "-242621399")
            .queryParam( "nonEmpty", "-670425985")
            .queryParam( "nonEmpty", "-93002050")
            .queryParam( "nonEmpty", "415934617")
            .queryParam( "nonEmpty", "126073958")
            .queryParam( "nonEmpty", "-751396076")
            .queryParam( "nonEmpty", "-438495229")
            .queryParam( "nonEmpty", "-423446425")
            .queryParam( "nonEmpty", "368833137")
            .queryParam( "nonEmpty", "-636370163")
            .queryParam( "nonEmpty", "-192573165")
            .queryParam( "nonEmpty", "-950503940")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_NullableItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "779116827")
            .queryParam( "nullable", "-687416322")
            .queryParam( "nullable", "-687416322")
            .queryParam( "nullable", "-189516033")
            .queryParam( "nullable", "690235927")
            .queryParam( "nullable", "915333261")
            .queryParam( "nullable", "1001400304")
            .queryParam( "nullable", "57032376")
            .queryParam( "nullable", "-884616211")
            .queryParam( "exploded", "")
            .queryParam( "nonEmpty", "1018882508")
            .queryParam( "nonEmpty", "-674126482")
            .queryParam( "nonEmpty", "-1063310230")
            .queryParam( "empty", "-10032195")
        .when()
            .request( "GET", "/query/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryArray_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-404501821")
            .queryParam( "nonEmpty", "-330932123")
            .queryParam( "nonEmpty", "-316988181")
            .queryParam( "nonEmpty", "-37708869")
            .queryParam( "nonEmpty", "-486985993")
            .queryParam( "nonEmpty", "604896465")
            .queryParam( "nonEmpty", "-316988181")
        .when()
            .request( "GET", "/query/array")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-1058884371")
            .queryParam( "nonEmpty", "-271415984")
            .queryParam( "nonEmpty", "-154202023")
            .queryParam( "nonEmpty", "572157103")
            .queryParam( "nonEmpty", "996514317")
            .queryParam( "nonEmpty", "-586465646")
            .queryParam( "nonEmpty", "861814351")
            .queryParam( "nonEmpty", "572157103")
            .queryParam( "nonEmpty", "997795463")
            .queryParam( "nonEmpty", "548205182")
            .queryParam( "nonEmpty", "-955614672")
            .queryParam( "empty", (String) null)
        .when()
            .request( "GET", "/query/array")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_EmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-519932006")
            .queryParam( "nonEmpty", "-937381090")
            .queryParam( "nonEmpty", "-652828067")
            .queryParam( "nonEmpty", "-44533766")
            .queryParam( "nonEmpty", "-72568761")
            .queryParam( "nonEmpty", "-684674302")
            .queryParam( "nonEmpty", "-673522965")
            .queryParam( "nonEmpty", "195851936")
            .queryParam( "nonEmpty", "-882105637")
            .queryParam( "nonEmpty", "735564063")
            .queryParam( "nonEmpty", "-400822681")
            .queryParam( "nonEmpty", "633376880")
            .queryParam( "nonEmpty", "735564063")
            .queryParam( "nonEmpty", "854231430")
            .queryParam( "empty", "CBg2d\"")
        .when()
            .request( "GET", "/query/array")
        .then()
            // empty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_EmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-1071044631")
            .queryParam( "nonEmpty", "-777855890")
            .queryParam( "nonEmpty", "-50042274")
            .queryParam( "nonEmpty", "-998087886")
            .queryParam( "nonEmpty", "268034420")
            .queryParam( "nonEmpty", "801516144")
            .queryParam( "nonEmpty", "1021786249")
            .queryParam( "nonEmpty", "-573290346")
            .queryParam( "nonEmpty", "261012018")
            .queryParam( "nonEmpty", "520338090")
            .queryParam( "nonEmpty", "-891759025")
            .queryParam( "nonEmpty", "481606890")
            .queryParam( "nonEmpty", "577902954")
            .queryParam( "nonEmpty", "-50042274")
            .queryParam( "nonEmpty", "246748580")
            .queryParam( "nonEmpty", "-584902327")
            .queryParam( "empty", "lfasqtdgnsydou,true")
            .queryParam( "empty", "502985810")
            .queryParam( "empty", "432246686")
            .queryParam( "empty", "775003968")
            .queryParam( "empty", "889044240")
            .queryParam( "empty", "44075002")
            .queryParam( "empty", "-1065000929")
            .queryParam( "empty", "-895277541")
        .when()
            .request( "GET", "/query/array")
        .then()
            // empty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-1064624872")
            .queryParam( "empty", "0")
            .queryParam( "empty", "-131013535")
            .queryParam( "empty", "-362061343")
            .queryParam( "empty", "-68168479")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-417805009")
            .queryParam( "nonEmpty", (String) null)
            .queryParam( "empty", "0")
            .queryParam( "empty", "528309323")
            .queryParam( "empty", "951994780")
            .queryParam( "empty", "-907519541")
            .queryParam( "empty", "-246655240")
            .queryParam( "empty", "366998639")
            .queryParam( "empty", "-934943784")
            .queryParam( "empty", "-71154022")
            .queryParam( "empty", "683050683")
            .queryParam( "empty", "-419806350")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NonEmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-978724008")
            .queryParam( "nonEmpty", "true")
            .queryParam( "empty", "0")
            .queryParam( "empty", "127687259")
            .queryParam( "empty", "-474172041")
            .queryParam( "empty", "-1061435334")
            .queryParam( "empty", "529194130")
            .queryParam( "empty", "320654082")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nonEmpty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NonEmptyItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-860553158")
            .queryParam( "nonEmpty", "-540383109")
            .queryParam( "nonEmpty", "-540383109")
            .queryParam( "empty", "0")
            .queryParam( "empty", "775662270")
            .queryParam( "empty", "476706974")
            .queryParam( "empty", "-357350020")
            .queryParam( "empty", "918750389")
            .queryParam( "empty", "112261482")
            .queryParam( "empty", "432312744")
            .queryParam( "empty", "607916881")
            .queryParam( "empty", "1056346689")
            .queryParam( "empty", "-131859165")
            .queryParam( "empty", "-517030818")
            .queryParam( "empty", "743536357")
            .queryParam( "empty", "56967725")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nonEmpty.Items.Size=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NonEmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-460973943")
            .queryParam( "nonEmpty", "-469.0")
            .queryParam( "nonEmpty", "966003976")
            .queryParam( "nonEmpty", "1008209210")
            .queryParam( "nonEmpty", "1047013775")
            .queryParam( "nonEmpty", "846383606")
            .queryParam( "nonEmpty", "1008209210")
            .queryParam( "nonEmpty", "573395544")
            .queryParam( "empty", "0")
            .queryParam( "empty", "528079217")
            .queryParam( "empty", "-764209970")
            .queryParam( "empty", "-886359421")
            .queryParam( "empty", "-635686961")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nonEmpty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "exploded", "-263864068")
            .queryParam( "nonEmpty", "-384662145")
            .queryParam( "nonEmpty", "74773304")
            .queryParam( "nonEmpty", "-971422379")
            .queryParam( "nonEmpty", "-651569829")
            .queryParam( "nonEmpty", "-971422379")
            .queryParam( "nonEmpty", "-439283806")
            .queryParam( "nonEmpty", "627219729")
            .queryParam( "empty", "0")
            .queryParam( "empty", "-41360311")
            .queryParam( "empty", "-859872662")
            .queryParam( "empty", "1046357294")
            .queryParam( "empty", "616308450")
            .queryParam( "empty", "-483488510")
            .queryParam( "empty", "-1071156057")
            .queryParam( "empty", "812313234")
            .queryParam( "empty", "-790310697")
            .queryParam( "empty", "-244454568")
            .queryParam( "empty", "-178778316")
            .queryParam( "empty", "-446624343")
            .queryParam( "empty", "716808335")
            .queryParam( "empty", "-1015034718")
            .queryParam( "empty", "-922194353")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NullableType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "6E'JU(8")
            .queryParam( "exploded", "-647330132")
            .queryParam( "nonEmpty", "-566436264")
            .queryParam( "nonEmpty", "68522676")
            .queryParam( "nonEmpty", "-566436264")
            .queryParam( "nonEmpty", "407064787")
            .queryParam( "nonEmpty", "-120204554")
            .queryParam( "empty", "0")
            .queryParam( "empty", "-949367959")
            .queryParam( "empty", "674203133")
            .queryParam( "empty", "-991865915")
            .queryParam( "empty", "748707165")
            .queryParam( "empty", "423831805")
            .queryParam( "empty", "43589412")
            .queryParam( "empty", "316717217")
            .queryParam( "empty", "873073606")
            .queryParam( "empty", "1025282594")
            .queryParam( "empty", "870300122")
            .queryParam( "empty", "-35154339")
            .queryParam( "empty", "-1042737142")
            .queryParam( "empty", "-287101809")
            .queryParam( "empty", "515363439")
            .queryParam( "empty", "739788595")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nullable.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_NullableItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "332.6")
            .queryParam( "exploded", "-426193033")
            .queryParam( "nonEmpty", "-376829912")
            .queryParam( "nonEmpty", "368659127")
            .queryParam( "nonEmpty", "-747842113")
            .queryParam( "nonEmpty", "877640584")
            .queryParam( "nonEmpty", "987236880")
            .queryParam( "nonEmpty", "588154396")
            .queryParam( "nonEmpty", "-694047682")
            .queryParam( "nonEmpty", "934770509")
            .queryParam( "nonEmpty", "588154396")
            .queryParam( "nonEmpty", "-493020903")
            .queryParam( "empty", "0")
            .queryParam( "empty", "-988586485")
            .queryParam( "empty", "-549008269")
            .queryParam( "empty", "-542867155")
        .when()
            .request( "GET", "/query/array")
        .then()
            // nullable.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "nonEmpty", "-792913942")
            .queryParam( "nonEmpty", "-985128916")
            .queryParam( "nonEmpty", "-723209788")
            .queryParam( "nonEmpty", "-290390595")
            .queryParam( "nonEmpty", "-19283938")
            .queryParam( "nonEmpty", "-19283938")
            .queryParam( "nonEmpty", "847647298")
            .queryParam( "empty", "0")
            .queryParam( "empty", "-275745314")
            .queryParam( "empty", "-699968594")
            .queryParam( "empty", "-954460197")
            .queryParam( "empty", "-562176900")
            .queryParam( "empty", "960723292")
            .queryParam( "empty", "115819170")
            .queryParam( "empty", "488764887")
            .queryParam( "empty", "1037743411")
            .queryParam( "empty", "-616396456")
        .when()
            .request( "GET", "/query/array")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", (String) null)
            .queryParam( "nonEmpty", "-878797277")
            .queryParam( "nonEmpty", "-701386008")
            .queryParam( "nonEmpty", "-74529999")
            .queryParam( "nonEmpty", "140594522")
            .queryParam( "nonEmpty", "243046145")
            .queryParam( "nonEmpty", "-701386008")
            .queryParam( "nonEmpty", "1053670037")
            .queryParam( "nonEmpty", "-831781577")
            .queryParam( "empty", "0")
            .queryParam( "empty", "523460091")
            .queryParam( "empty", "933318457")
            .queryParam( "empty", "833131836")
            .queryParam( "empty", "-78556733")
            .queryParam( "empty", "908992362")
            .queryParam( "empty", "-443126775")
            .queryParam( "empty", "-433697934")
            .queryParam( "empty", "439911168")
            .queryParam( "empty", "809309101")
            .queryParam( "empty", "-870502987")
            .queryParam( "empty", "931718676")
            .queryParam( "empty", "94289949")
            .queryParam( "empty", "1000117139")
            .queryParam( "empty", "90295147")
        .when()
            .request( "GET", "/query/array")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_ExplodedType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "chmsxgaiffxwjdv", "%2Z7Udz),MKq)8$")
            .queryParam( "nonEmpty", "-837203869")
            .queryParam( "nonEmpty", "-1046568058")
            .queryParam( "nonEmpty", "-114984877")
            .queryParam( "nonEmpty", "824744969")
            .queryParam( "nonEmpty", "124911247")
            .queryParam( "nonEmpty", "124911247")
            .queryParam( "nonEmpty", "140565933")
            .queryParam( "nonEmpty", "-325241725")
            .queryParam( "nonEmpty", "-961763562")
            .queryParam( "nonEmpty", "927178423")
            .queryParam( "nonEmpty", "-41576849")
            .queryParam( "nonEmpty", "72410734")
            .queryParam( "empty", "0")
            .queryParam( "empty", "420665591")
            .queryParam( "empty", "78627739")
            .queryParam( "empty", "-925940599")
            .queryParam( "empty", "-989813468")
            .queryParam( "empty", "-452680549")
            .queryParam( "empty", "-111445797")
            .queryParam( "empty", "-282720798")
            .queryParam( "empty", "962555649")
            .queryParam( "empty", "-123731175")
            .queryParam( "empty", "-803554183")
            .queryParam( "empty", "550846493")
            .queryParam( "empty", "-350584145")
            .queryParam( "empty", "536497289")
            .queryParam( "empty", "-576321328")
            .queryParam( "empty", "-624055355")
        .when()
            .request( "GET", "/query/array")
        .then()
            // exploded.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryArray_ExplodedItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "exploded", "-508.1")
            .queryParam( "nonEmpty", "-697861720")
            .queryParam( "nonEmpty", "-399027577")
            .queryParam( "nonEmpty", "-558516118")
            .queryParam( "nonEmpty", "624177041")
            .queryParam( "nonEmpty", "-558516118")
            .queryParam( "nonEmpty", "648756300")
            .queryParam( "nonEmpty", "-669106153")
            .queryParam( "nonEmpty", "-129272108")
            .queryParam( "empty", "0")
            .queryParam( "empty", "892919141")
            .queryParam( "empty", "469899679")
            .queryParam( "empty", "490913067")
            .queryParam( "empty", "-846139301")
            .queryParam( "empty", "74017987")
        .when()
            .request( "GET", "/query/array")
        .then()
            // exploded.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", "0")
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[t]", "true")
            .queryParam( "deep[vmu]", ",,,~6E'{_>")
            .queryParam( "deep[xyr]", "'k")
            .queryParam( "nullable", (String) null)
            .queryParam( "width", "0")
            .queryParam( "height", "0")
            .queryParam( "ibezcj", "^P@xz&}?")
            .queryParam( "width", "0")
            .queryParam( "height", "0")
            .queryParam( "dnztec", "[0E(:bV,")
            .queryParam( "vx", "450")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", (String) null)
            .queryParam( "nullable", "width 0 height 0 vkkomzdvjqqsojnl ,hkK g -223.5")
            .queryParam( "width", (String) null)
            .queryParam( "width", (String) null)
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", (String) null)
            .queryParam( "deep[awxwxohm]", "")
            .queryParam( "deep[ibvi]", "484")
            .queryParam( "deep[dxivpimginhnn]", "fdrnomrlex,true")
            .queryParam( "nullable", "width ")
            .queryParam( "height", (String) null)
            .queryParam( "fsrevwvibggt", "B")
            .queryParam( "qh", "gigmaypvesiyzb,{,kihoxj,460.8")
            .queryParam( "width", "463221856")
            .queryParam( "height", (String) null)
            .queryParam( "q", "117.9")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", "192270469")
            .queryParam( "nullable", "height  mpkbvvosossjwsw 963.6 zblylgbd true ypttqjfrilug -889.7")
            .queryParam( "width", "825949174")
            .queryParam( "height", "754260488")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "509037079")
            .queryParam( "deep[vkwesr]", "/mwM`,.BR0A#!")
            .queryParam( "deep[sjkgx]", "DbmFZ;")
            .queryParam( "nullable", "width 467318146")
            .queryParam( "height", "63782742")
            .queryParam( "lzlgihskrmr", "-626")
            .queryParam( "flupfczfyqikxkmk", "")
            .queryParam( "mycnrkokregxwxdu", "680.9")
            .queryParam( "width", "0")
            .queryParam( "fqqurgc", "true")
            .queryParam( "qlmatldlbgnmlcfz", "765")
            .queryParam( "bhmjwfdq", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", "0")
            .queryParam( "nullable", "height 556362815 ljsbkwqz  bbptfpytc pE!Pn?3 ibptsudv ")
            .queryParam( "width", "0")
            .queryParam( "height", "0")
        .when()
            .request( "GET", "/query/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[ztpjurulvrjpy]", "K4")
            .queryParam( "deep[bco]", "")
            .queryParam( "deep[nznrzwbqxibipkj]", "true")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "drocdtzp", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[byrjncljsrfgnsz]", "yyjloqdswuoqm,-819")
            .queryParam( "deep[fj]", "")
            .queryParam( "deep[vfc]", "-161")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "kpollw", "739")
            .queryParam( "oamo", "")
            .queryParam( "nonEmpty", (String) null)
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[jvsbkmwthgjsioqe]", "eimgdokpxsjxige,true,xysmtfokculr,true,vljg,-386.9")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "ykouuvo", "759")
            .queryParam( "zbspwdlfzdfwar", "-113.1")
            .queryParam( "nonEmpty", "true")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[vmsknkcapwftptb]", "hyhz,#U_+$f")
            .queryParam( "deep[lzcginjmhkjzjdd]", "150.8")
            .queryParam( "deep[rqkqpotnrfnmbvwj]", "495.5")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "zfcqisrpcpqsnyn", "")
            .queryParam( "width", "V67l'D")
            .queryParam( "hfxlbpgp", "2bSG0g")
            .queryParam( "rpy", "-139.5")
            .queryParam( "rshf", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[lgkcg]", "408.6")
            .queryParam( "deep[rbdwnkkzbppmcgj]", "")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "ywrbgtz", "")
            .queryParam( "vkosmbpawlmoc", "-995.9")
            .queryParam( "d", "-976")
            .queryParam( "width", "-1")
            .queryParam( "digorzef", "Kjd")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[lxpne]", "true")
            .queryParam( "deep[maaihxpxoqemkg]", "}OtD")
            .queryParam( "deep[hzawq]", "true")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "iwmpcshvz", "")
            .queryParam( "width", "269782820")
            .queryParam( "height", "true")
            .queryParam( "qjnvm", "-:}'OT7")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[qbjeg]", "823.1")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "wvkwjfngaflvim", "true")
            .queryParam( "width", "963914998")
            .queryParam( "height", "-1")
            .queryParam( "ufa", "394.3")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nonEmpty.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[pz]", "HA\\")
            .queryParam( "height", "0")
            .queryParam( "icanhaerefsox", "-914.2")
            .queryParam( "width", "444746474")
            .queryParam( "iazicdkhj", "true")
            .queryParam( "akxauecczize", "-183.5")
            .queryParam( "uxdtpabdyy", "398.3")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[d]", "true")
            .queryParam( "deep[ytcy]", "qtxtzck,-160.7,wguwoyfqfoe,true,eahxydxcahrlsjv,X`sd3")
            .queryParam( "deep[gycgxzg]", "hyl,]hjD7,a")
            .queryParam( "nullable", "O~Q38QS")
            .queryParam( "height", "0")
            .queryParam( "wnywhjfqmrfxoazl", "qamxkhyeblmbs,a")
            .queryParam( "width", "268037975")
            .queryParam( "mtss", "625")
            .queryParam( "ninfbrcc", "401.8")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[zlhhovnskmtqnuzu]", "w)v")
            .queryParam( "deep[ehf]", "-152.7")
            .queryParam( "deep[rbldzbskvujys]", "-548")
            .queryParam( "nullable", "width true")
            .queryParam( "height", "0")
            .queryParam( "mvdqtrkwyje", "3p,e,")
            .queryParam( "yax", "768")
            .queryParam( "ywozlfqgrw", ",Awt0K,3L=D")
            .queryParam( "width", "388066613")
            .queryParam( "ruatbvtink", "true")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[sqvwpshfzuq]", "-622.6")
            .queryParam( "deep[okqfdgdgcqziyof]", "true")
            .queryParam( "deep[njmkyy]", ",`UPTIE$A")
            .queryParam( "nullable", "width -1")
            .queryParam( "height", "0")
            .queryParam( "dcpxmcqlalhx", "+$@wZ")
            .queryParam( "rkn", "tihfynosn,646,pe,-238.4")
            .queryParam( "width", "515683667")
            .queryParam( "iahogihezbuaxgey", "-674")
            .queryParam( "gzufuiowtvsgzw", "234")
            .queryParam( "cydlvugrelysowng", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[jslbecjhjroyjjm]", "gvjeaealwljabcq,305,kiypom,-849,dhfzndymjwkdauoh,-997.6")
            .queryParam( "deep[dvlu]", "b")
            .queryParam( "nullable", "width 0 height true")
            .queryParam( "height", "0")
            .queryParam( "k", "qz`&Q?,%tIK]J,")
            .queryParam( "r", "/W,yoq\\.U`")
            .queryParam( "dztmlqlbitfm", "true")
            .queryParam( "width", "660225892")
            .queryParam( "tfjz", "-837.4")
            .queryParam( "jrpdztpopazuy", "wnhwjvqg,-179,vzzmjxpsdij,SOFxIfr,qozasjmvinhptig,:uH$clo")
            .queryParam( "f", "true")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[walg]", "\"JPJ5Ye")
            .queryParam( "nullable", "width 0 height -1")
            .queryParam( "height", "0")
            .queryParam( "slsa", "xpktkrkfjrixlo,D_@7&s,mbnkc,52.3,gcfurnjuzwte,230.5")
            .queryParam( "width", "930797269")
            .queryParam( "vmdikuyaczbsed", "250")
        .when()
            .request( "GET", "/query/object")
        .then()
            // nullable.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[hbubmycswmddr]", "true")
            .queryParam( "deep[mp]", "true")
            .queryParam( "nullable", "width 0")
            .queryParam( "width", "971056236")
            .queryParam( "qqevpqphgkdyru", "true")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[eucjmben]", "")
            .queryParam( "deep[zzmijdktvduee]", "*\\")
            .queryParam( "nullable", "width 0")
            .queryParam( "exploded", (String) null)
            .queryParam( "width", "518579809")
            .queryParam( "airvqcrkzgyfjm", "buejg,true,niu,439")
            .queryParam( "tulktscabpr", "")
            .queryParam( "grsn", "xC")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[tqnisxfsnt]", "k")
            .queryParam( "deep[oiecrhwxbrd]", "wo,-889.1,bym,-896")
            .queryParam( "nullable", "width 0")
            .queryParam( "exploded", "true")
            .queryParam( "width", "1073380189")
            .queryParam( "wjsulzzidxaqd", "")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[jwfdwp]", "Qi.X)p|`,#XkY[")
            .queryParam( "deep[wywdfsgspmv]", "-1023")
            .queryParam( "deep[agyuipfjtqdlqfz]", "true")
            .queryParam( "nullable", "width 0")
            .queryParam( "width", "th,true,wygeakydkkmwfwm,-949,mmbiolatb,uUr%$,,m=]z")
            .queryParam( "height", "0")
            .queryParam( "pbidewwwugnv", "")
            .queryParam( "lqoaugwjidazmq", "true")
            .queryParam( "width", "996246710")
            .queryParam( "wtjxerrfwvc", "!WOv.")
            .queryParam( "cz", "-895.8")
            .queryParam( "rcoojo", "924.5")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[oovwrflfcrwd]", "true")
            .queryParam( "deep[nlsnjlofchenzdp]", "-775")
            .queryParam( "nullable", "width 0")
            .queryParam( "width", "-1")
            .queryParam( "height", "0")
            .queryParam( "v", "imu,iXXLfB")
            .queryParam( "width", "964600268")
            .queryParam( "cksqz", "-963")
            .queryParam( "bx", "rPv/,!,2NY")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[pmcocxpokw]", "A][B")
            .queryParam( "deep[ld]", "Y")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", ",M,*WazDV")
            .queryParam( "fqhb", "true")
            .queryParam( "width", "739058877")
            .queryParam( "dmbufl", "t,-516.0")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[wskmziape]", "920.6")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "-1")
            .queryParam( "wyjqqdzakpu", "Qyg\"r2:,S)#,3R^Lk")
            .queryParam( "ppzbuofpj", "-155")
            .queryParam( "cxnrz", "true")
            .queryParam( "width", "863964201")
            .queryParam( "eh", "TB#J>U,]%+,vtHp$F")
            .queryParam( "lweyndokmmixg", "-926")
        .when()
            .request( "GET", "/query/object")
        .then()
            // exploded.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "vlrxfgfja", "true")
            .queryParam( "lz", "637")
            .queryParam( "jaltgzpdy", "true")
            .queryParam( "width", "406997898")
            .queryParam( "vheeftpxtkln", "zbeszarqwxpf,f ")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep", (String) null)
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "znnflqokbrjygb", "I*mIf`Ij")
            .queryParam( "bl", "sv~'Y")
            .queryParam( "oglbsndcw", "true")
            .queryParam( "width", "800250155")
            .queryParam( "xm", "fqfzhwbnfjnid,-261,ihhfzx,R@T,9a^NIzCW,tcqerwggkk,f])m{,")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep", "true")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "bp", "-2")
            .queryParam( "woetlbnpfgfkgmnh", "19")
            .queryParam( "width", "151856373")
            .queryParam( "nuxihci", "jqdjdcgrljwuqgr,true,ynzhkzvzziipp,-453.2")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", "74.2")
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[nibjhdbyaug]", "769")
            .queryParam( "deep[tsxlhpxlkoil]", "_[{zNs,8Xd,c")
            .queryParam( "deep[ssfmzxs]", "-94.9")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "jvsifqsbx", "true")
            .queryParam( "abklm", "sPFAIT^C,&Y3{!,1rS.?UlY")
            .queryParam( "rzighsjzvmqpt", "394.8")
            .queryParam( "width", "495513695")
            .queryParam( "fphtaexzj", "dfbfgwjudcwc,75.8,mxfljkus,8YgUl,wlcckvpbcq,-366")
            .queryParam( "vzd", "615")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[width]", "-1")
            .queryParam( "deep[height]", "0")
            .queryParam( "deep[oxraxfil]", "3'XuWaQ")
            .queryParam( "deep[ucylnj]", "723")
            .queryParam( "deep[csnhl]", "")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "zi", "nssjdwl,55,sffxaoqafpye,-540.6")
            .queryParam( "width", "275326498")
            .queryParam( "pgxuljek", "true")
            .queryParam( "tbgdvoalqctbfd", "$g8{z'{")
            .queryParam( "w", "9[,a.:k#")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "")
            .queryParam( "deep[bepbpacosxyotq]", "")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "xnvh", "55")
            .queryParam( "width", "1050319604")
            .queryParam( "nwgrywfzriwnt", "-590.6")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryObject_DeepValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "deep[height]", "-1")
            .queryParam( "deep[ojirukpcbixcw]", "urixuznltyorxuw,856")
            .queryParam( "deep[okjjmqfnjww]", "qwoeptmghzaepr,-725,nv,m)%{1LG,A,s $@87,gl,,(")
            .queryParam( "nullable", "width 0")
            .queryParam( "height", "0")
            .queryParam( "sucsxjatsvqxp", "")
            .queryParam( "s", "337")
            .queryParam( "width", "382094581")
            .queryParam( "gzkwbyjzsoc", "191")
        .when()
            .request( "GET", "/query/object")
        .then()
            // deep.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wz")
            .queryParam( "nonEmpty", "x6{")
            .queryParam( "empty", "h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=x\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*a")
        .when()
            .request( "GET", "/query/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryString_EmptyValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", (String) null)
            .queryParam( "nonEmpty", ")@b&1avN2IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^)1=$}o%zg")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryString_NullableValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "")
            .queryParam( "nonEmpty", "\\bD")
            .queryParam( "empty", "dep5FtnA\":SD4:uw,pGY`UWA9GqrV'Au!L.sq*Pf%z&b?lK{2hFEDwG[AcSVW%M'|y3{f5xCL8-Cu|k*Ia9A+*A,|L?-G=V]L8CYg t<P]#{k%AlU1l`p?H|Q~4%@~[NgZ> 9M3>cv.$m]f7qH[r3.GC#rU f7R#?y[aaJ#Y:%`$s(A{@\\@s<\"M5rH[vC?>7(^zKpUPb{ CO2qR}\\Dk iFXrX\"XH&hN'7dl-FE7F$i?^G*Q\\N")
        .when()
            .request( "GET", "/query/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getQueryString_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "x*:/3GOUKk5L\"y^5as@3O-;]90Q2kNWES$tna")
            .queryParam( "nonEmpty", "bbEvzR;]Y#'(0I")
        .when()
            .request( "GET", "/query/string")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "{Lr>ddf523Cj{(#ur5@!]i@")
            .queryParam( "nonEmpty", "Nx2T>[ T.n+4e/QXQFoMTvrLW#X}")
            .queryParam( "empty", (String) null)
        .when()
            .request( "GET", "/query/string")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "P{C_2`OKs914cJYO&=(T>{hfh'##I/*uigZ6GmM&Pm%` %DKf!jA,:\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%rt)list")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/string")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", ">?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_")
            .queryParam( "nonEmpty", (String) null)
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/string")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_NonEmptyValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nullable", "V|=")
            .queryParam( "nonEmpty", "KN")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/string")
        .then()
            // nonEmpty.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getQueryString_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "nonEmpty", "ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLD")
            .queryParam( "empty", "")
        .when()
            .request( "GET", "/query/string")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
