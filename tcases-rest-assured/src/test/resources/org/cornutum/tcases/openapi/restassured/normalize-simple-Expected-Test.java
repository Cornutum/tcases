package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class NormalizeSimpleTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getHeaderArray_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", "-153258607")
                .header( "nonEmpty", "-164078794,-583719456,135686919")
                .header( "nullableNonEmpty", "")
                .header( "empty", "-157088910")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", ",72445273,930840690,-488003572,-156217896,618632926,662269978,-506613678,-721157401,628449427,862231626,-827445365,-309047286,-812171476,-744566098,362863255")
                .header( "nonEmpty", ",965599236,-1041204659,567874896,-811599341,-1041204659,-578228129")
                .header( "nullableNonEmpty", "")
                .header( "empty", ",-346446824,-528280770,386245946,-672634311,-583462575,545605436,649506378")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "-879143436")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-344642009,564413979")
                .header( "nullableNonEmpty", "-507997290,19558260,-431026762,-886223838,84904659,1034989629")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", ",30011471,-26623488,49210815,474091083,-832283669,369021225,275892753,-158133855,-68051223,-1043909567")
                .header( "exploded", "418710350,-420732884,297193076,-25489855,359040639,297193076")
                .header( "nonEmpty", "518899463,88593603,-952388200,173770038,518899463,-1006821129,523343652,156259055,887063415")
                .header( "nullableNonEmpty", "")
                .header( "empty", "954940534,954940534")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0")
                .header( "exploded", "-727019885")
                .header( "nonEmpty", "-702616317,-655844556,-627332016")
                .header( "nullableNonEmpty", ",-800089729,-404706851,")
                .header( "empty", "-863369441")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "204365502,35927882,930579254,781848857,-419826914,35927882,754954234,-380282749,-277435191,-849518397")
                .header( "exploded", "0,-422305019,-280951260,-244050967,502860115,38901031,936150878")
                .header( "nonEmpty", "0,720628247,720628247,426875809")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0,-983591672")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", "245677325")
                .header( "nonEmpty", "1066059931,-989766256,81784674")
                .header( "nullableNonEmpty", "0,-919816221,520579710,59094201,-244290827,439594403,-203498227,-1067818880,623121701,578649117,78751424")
                .header( "empty", "190679896")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "-897508237")
                .header( "exploded", "-921970944,678906239,-921970944,490939833")
                .header( "nonEmpty", "-106064671,-623570711,34423693,230199745,-18066782,-868318486,-561360787,734588497,460791355,716027810,230199745,-798415326,-916621891,-86741364")
                .header( "nullableNonEmpty", "16922511,703247454,-317446460,118799401,-134514511,-317446460,730474884,-557523998,-118104854,-341761009,537072060")
                .header( "empty", "-53924364,348050097,-207864835,478164266,623691266,646398474,-901229922,-805814753,774386953,-985480644,727671143,-944823569,706731548,-805814753")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,738344007,627296853,-311278904,-1063417609,-83134143,-756642214,899582039")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-770166788,-435639444")
                .header( "nullableNonEmpty", "")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,404774792,58208165")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-957060495,-776646755")
                .header( "nullableNonEmpty", "")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,691972744,673698912,-380184617,-474989394,-255862890,351939176,894198957,-93501521,104806809,360398643,1045124066,331039137,230722781")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-499109668,-353663695")
                .header( "nullableNonEmpty", "")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-512302342,-847163316,-896438212,-270332143,-234906661,462423158,-325409180,-850724430,-917808615,464385673,890085130,617146312,290427985,-528826170")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,87876097,-1065004142")
                .header( "nullableNonEmpty", "")
                .header( "empty", "UdN6Dq")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-975588857,-304884824,870779198,-1019780051,562153208,740312557,-142699090,-360452629,120754232,-178192870,-521654952")
                .header( "exploded", "0")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-452095163,67119055,-135385010,1034883342,-584141731,-414145721,1060861624,-575959981,273669133")
                .header( "exploded", "0")
                .header( "nonEmpty", "")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-776889751,989857126,-422761640,-564535243,384565277")
                .header( "exploded", "0")
                .header( "nonEmpty", "-680")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-329778607,990295629,683798131,760985355,674627519,-1066673733,936151253,658591365")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,0")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,731348640,-750047856,-1005097923,715651995,-242423866,-147302448,-980202951,110848895,-882947971,426059454,-182315734,298480677")
                .header( "exploded", "0")
                .header( "nonEmpty", "oqoovfobddhyrk,791.7,yikvn,559,csxjatsvqxpvto,-413.0,-691647243,-525219417")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nonEmpty", "0,340607161,267850201")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-452527857,-977445778")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "-889.4,-942347065,1041506275,-574802857,-956543955,-961224299,845201056,-139757207,953951780,569444104,562391096")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-1040945839,335636564")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-292700727,964303356")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,107348705,397221992")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-838684499,450810644,490125460")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,-123281351,20731913")
                .header( "nullableNonEmpty", "WT")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-869287546,-210004624,-1070674266")
                .header( "exploded", "0")
                .header( "nonEmpty", "0,595294443,61404077")
                .header( "nullableNonEmpty", "&tTXg8@-,500059247,-585017347")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-415596357,952161561,-991640072,-508296392,708290815,672651064,942985147,-887075963,-380633963,752444287,-297785017,160310445,-295829014,-835276471,544268945")
                .header( "nonEmpty", "0,336632291,-417811498")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,838146559,531273906,98379363,-134679646,866329582,-644286929,42890667,-990739677,-68724368,660561086,-537755425,-161990624,-1042620397,-124121987")
                .header( "exploded", "")
                .header( "nonEmpty", "0,43520868,-169782167")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-460654512,-12894697,863181495,197436499,569862538,300335793,-550661280,619616696,-472648652,-253914298,-952261962,391548755,856630782,-859469848,915663614")
                .header( "exploded", "bvytefdjufrgnpy=6*@fu>u")
                .header( "nonEmpty", "0,454401613,-414961680")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,652241165,874225736")
                .header( "exploded", ".,,(x~m?~bA")
                .header( "nonEmpty", "0,986205292,1023639635")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", "width=0,height=0,mcxbxzod=783.8")
                .header( "nonEmpty", "width,0,height,0,y,etiojmkkokc,948,fkhcfzpxyxisupfw,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "exploded", "width=")
                .header( "nonEmpty", "height,")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,height,0,nfknvtpfxnhh,true,uxyqhoxrz,")
                .header( "exploded", "height=,kxyjk=true,qcmcyibbjfsg=;JI=jmuT,erfxitoh=")
                .header( "nonEmpty", "width,,ndvkqduxkse,181.0,cwzhls,-726,lqwvb,gasindc,true,jxkdhapqbhiaiiw,true,fqvwobdcgr,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,")
                .header( "exploded", "width=314515493")
                .header( "nonEmpty", "width,818628085,height,1028028666")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,51494515,height,,phipkgdb,true,nbsqkjuwkkabrync,-O=xzJ`")
                .header( "exploded", "height=457026643,k=/@;!1")
                .header( "nonEmpty", "etsgjcd,true,nnrhyuybo,-682.0,wol,")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "height,79038382")
                .header( "exploded", "width=0")
                .header( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,qnxw,true")
                .header( "exploded", "height=0,j=uzvstxi,-68,iwhqfbjpxvxlc,-530,wobdblvrcimdzbzr,-249")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,argmbykuivc,@:BGZ'^")
                .header( "exploded", "height=0,xhieoahttn=true")
                .header( "nonEmpty", "")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,fyhbqzspqt,kPo&)")
                .header( "exploded", "height=0,sxhsrprcoddlzs=uraetsuzukjbhzt,FqG##")
                .header( "nonEmpty", "bY Gx,S]%2QH>")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,hhzojduuthp,true,kwk,-140.0,rloplu,true")
                .header( "exploded", "height=0,ujlwryyhq=,zjvkjddiaspjuj=prouvkuusefizgzi,true,vhkpxenmrvrgjpr,,zMBg$l,m]C2W")
                .header( "nonEmpty", "width,<u{\",&#~<,w5w5,kesgvdwazxltwi,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,prozfl,756.7,lctbaw,-235,dopqfvkxhade,")
                .header( "exploded", "height=0,ztxyptdirjjfrj=-581.5,dauruohhqh=kfxmeigni,-79.5,hsdedql,<#")
                .header( "nonEmpty", "width,-1,auqhntkpjud,-265.0,prinyxtmia,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,femgzvg,RjuU6&,bhbvticwb,820,qkfpptiwgbjaigpe,true")
                .header( "exploded", "height=0,jzbvg=906.2")
                .header( "nonEmpty", "height,,?a,],nutqihpn,,upxiuhowxd,-876")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,q,967.0")
                .header( "exploded", "height=0,uxundmtb=true,yicpazagrxjikl=,gqqsvzmxmlbkoyvz=true")
                .header( "nonEmpty", "height,-1,mwbkmpdsrx,371.4,jur,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,bpxriwzbbreu=,`'$")
                .header( "nonEmpty", "ciwa,946,ihlbcqlm,a,nksizirc,t~Q-)%h,[-B!iN?")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "true")
                .header( "exploded", "height=0,bnchczxgp=true")
                .header( "nonEmpty", "hpkumyiyghyan,-209,bhdjqlilofpc,U")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,rhx-sgp],vwgxijbduvng,true")
                .header( "exploded", "height=0,whsyvdwxeqte=_g[x:,0N,Yp{0,rfyjg=.C!7")
                .header( "nonEmpty", "jljkayyivqlx,,tq,")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,-1,llerzxmcnrga,o,,gzocwqcmeuiiiqw,288")
                .header( "exploded", "height=0,qgf=`<T;n=")
                .header( "nonEmpty", "gwezrmvbb,693,ajhmxndign,-264,xgrxdgyqyveokzv,rhhhclbpp,-316")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,height,true,wevffgwcqqhv,uimtncbraltgkr,true,piv,-701.4,lflwwlxrlm,-578.1")
                .header( "exploded", "height=0,ezccpvwdueb=true,ueirwg=true,mpawepgi=49.5")
                .header( "nonEmpty", "hnjo,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,height,-1,e,gvatzbhvordn,-215.7,pklf,-344.7,rjlhibbmv,htioix,U,iuslsalrqhmvytox,,jlwtlyolrnojz,Q81")
                .header( "exploded", "height=0,ehgxhxzxyy=u,fxsdnozfy=true,nxrxensqhjxe=130.8")
                .header( "nonEmpty", "vuuokn,^M:^!jw,,.,,Sa,urdeuxryasb,-199")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,iauizitn,,oziafkgti,NW,GS3Uuv|,+>B,qxffmmwqyojrwkgn,true")
                .header( "nonEmpty", "g,3<Qqy_Bf,qgjrvtauyzrmevr,]I7g2D,]8i/o,hx\\\"S")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,fndqa,true,aclkfss,,joedya,true")
                .header( "exploded", "")
                .header( "nonEmpty", "rpkxaeejgcz,")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,xjqqgt,$u,aq,cd,>,fsznkyuajqd,-1019.4,caytupxrzaghe,j7JW,nlfgasekinwtly,opjzdcmu,374.5")
                .header( "exploded", "7hdXR*[s,")
                .header( "nonEmpty", "yypjw,^w^eDrJ6,bdjkh,683,gcvly,otdimhz,nr_*vfd")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,zzjxznmndte,,ifenrldmcbhdstn,true")
                .header( "exploded", "width=,height=0,wnhoptll=;yVA,nlgvuvwpjinscc=dKXW=z_,JwzTpz,N{zi&")
                .header( "nonEmpty", "d,?3L6GUZe,:`j6,\"")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,fgylekhl,-875")
                .header( "exploded", "width=-1,height=0,vaouzlzqfoegc=955,a=308,lgonfpdrbju=true")
                .header( "nonEmpty", "wheojceio,,foeqbfikmtdfncef,vcuwurzeishkgqj,OQk n2R")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,rnaosldnk,-890,sxgtecik,;Ss")
                .header( "exploded", "height=,TGLs*B0,mzum=|e_i'\"ss,%dp<P,NpHBWM].,zsbjhajpftzjbgem=-922")
                .header( "nonEmpty", "dzghhbfxhze,true,mlas,3%,fzMV,schqkn,-109")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,fgrxfnebpkyut,true,fvmbotsplcbhc,511")
                .header( "exploded", "height=-1,nivpwodyiqskt=rvtwhqqbye,`ud,b+6@,bpweeanzvkzvkznv,,pziwa,-404,a=dmzihzfldn,336.1,rjbnpra,-585,qydstkmy,-245.9,pduksjsdnmc=prrvwzdnvo,true,ennk,true,ek,P")
                .header( "nonEmpty", "iutdgte,-154")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "f!jA,:\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;")
                .header( "nonEmpty", "u#>")
                .header( "nullableNonEmpty", "")
                .header( "empty", "D")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nonEmpty", "y:o27HkDZ%rt)listU>?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsS")
                .header( "nullableNonEmpty", "")
                .header( "empty", "Ti|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09rscxxp8%X/#>V|=KN(ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nonEmpty", "pP)")
                .header( "nullableNonEmpty", "oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLDewSA>qQZ")
                .header( "empty", "=")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "bnm^hT=[M~LM]2iAtQ*{M*H< yoM\\Z<]t%QO{7_`&pEdwsXr3Fgmd J| =o_oc=[`Pal#.G`.4{wREEbl'H<q%|EdyD")
                .header( "nonEmpty", "Pp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~ D\\X2>_qzE`@c% Kg,)xak\\Q:bcGrj6(F(g8\\'gX'uf8ui~Y+F>-X(mg(f3NDGZW$sU\\l,8d")
                .header( "nullableNonEmpty", "")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "4Ush*wg5cupL@=wy5")
                .header( "nonEmpty", ":7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U")
                .header( "nullableNonEmpty", "")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "{HM/mwM`,.BR0A#!Z3Y!}Zg4DbmFZ;sjRz4\" X}x3Bpak+~?D:?5j_D&;leD)#wGUu?pED[.7c/\\B~PaT\\:4x7j+(}Tn[?V50smO.%s8!QXwg~b*]ALUF&CH~0V_Pd`-3xbZmws?v2Rpj/f1^i^2iVySVmj8~t++\".1L9Mk_d~2x&q}7Lf19C-A Kg~")
                .header( "nullableNonEmpty", "")
                .header( "empty", "4XfU^7i&{<p94(=j4*lDd@=q| o?%]")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "\" rPt/*vom*\\V:v/&5fr=P=Wa~s''#q\":CoWP^zw`E)Ss?gt@T+8''!Q7[>W$}xlw5C6xw}n975cB9C7d<1v[4&13(7giO~HtA7GA@;UM&<y/c^PS{u{j>X+c+mz=(BJ`)Q%Y?C~r2Y$Q^:~]7kY~)2XReg230od3D2R%rXN$wvKQOG\"UBx{D|=G#U_+$\"")
                .header( "nonEmpty", "")
                .header( "nullableNonEmpty", "")
                .header( "empty", "F/-~Wv^Ij3QRe]QgSxqqIFk\"Y[cn<w:EYv3qSUy>{V@DVZ#y?]>?H^YNRWo,V67l'D{8AvIQT;wrW_2bSG0gNzVEBzAVheOMT3RzN`fTSro.#'Ql*w'|x)7 i+Fs/fIf)U^d)oG\"y0ls;+4F0T?")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "H_}OtD^*b}mMlhvG/h+kgJ1|Rx^X<fl0>UO! -:}'OT7c(VNF8W6H@5HYFGoE`c@T\\Gq7oQ^LQ_kr{l~f-$mJSHA\\.TsZ#y,Rd>5V+#^f(Fn2!IV^~0]\\K#|k%mTt5y_bI&q:q1a ]DkTwZ")
                .header( "nonEmpty", "x7")
                .header( "nullableNonEmpty", "")
                .header( "empty", "AD3m,>56P~)=\\&QQsh0pC^0iZ][l9hgky;oYMI!>2AoP<x&V'tL2DG:tZX`sd3(d#D_jIn=h;hylj]hjD7SaoIG0PxZFw4IEWD78](;}-T~CjbL+qt:p_74B*rt)(Tl`faB*GO|3>~N:L.WROM U[)6t|]a]_`hvb;.k(ov:4,\"/w)v]B`VV /n\"LB0MuH\"Vt]W`{J{;Z73tB}7LL2Z}+iO3pFe\"")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nonEmpty", "6&7XQZm85j,n,O<w kD)Awt0K93L=D4r>!=d`mRox8mx@e$0`6H~iAJ{,XtT=@)4W8r<_*]WTp_9;b?2:piu4RDl`UPTIE$A9=KuGLSrg}M.fErV+$@wZ$v7%D\\z5soZ<Hi|o^EIU!<rs+jt<}sEKs0:]x`Qv$S(P\\Q~Qkr]6L>}DXTz k$O")
                .header( "nullableNonEmpty", "")
                .header( "empty", "pt.z<#82?;$URwF$ VpxtPj\"aye\"\\g:hAWO#!{HloWxb$_`[x'0,m}qV$6|@WwWnU;KcSda5m\\l NDt7<jmHGOIwc*Eb+3;L=Axqz`&Q?D%tIK]JG+/:?U/WIyoq\\.U`o'gMllr+uCfq[O5+\\~c!%U~WkmaNOR+^*I!`zy}pD<,A'3@3h\"|uXQNd8yCU)ow`xSOFxIfrc] 7vY.YGU|Kc'2e)_:")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "IU=?KXIcQ*)(LRO=;Prs$(7hM+*Y kq>ueSUy ,wr2B*~x_x!ZFAb@QO>sj=IV'r?#C|*\\nk\"CS$4]BVTuU@R0Gq8xc}'mJq4o7D;WVKqu*6CV&=,8mrKV{;c+.xCN%\"_}s FcMKg9ok`=Plt6J~_fOgLwQ9IPH4v9}9|G!?Sdkj6i'o$1JrJ96'pyFgS=")
                .header( "nonEmpty", "'4!1H~Qi.X)p|`2#XkY[5{#_@&\\$<N]K=Q'F4#R*-j\\m\\j|j/3!ZfYWo{qEFoZ)DiMa(K/\\DW;u*j\"C?lsNptCBOuUr%$^|m=]z I3SY]ud.c3~SAY'D[yHHC'd)5x|Q4M*|:s;2C]6*W`hSOgR!WOv.`NVW~bmLQ")
                .header( "empty", ";xE\"vuk *Tgu%?0VVt+\\{/hLP0GY/@xG=LFzsm;bVCvW/tcT8ViXXLfBR=eR6e]+kuUbl[\\nrPv/'!92NYcqkO(6dDZ9^~_!BA][Bx[[Q/YW9\\y:(MN*WazDVopn:{%!+H}aumWV^l*r$A)K@1r7Wx!hRflRfGvf9X3//C|6h:Ci,~Qy")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-915629898")
                .pathParam( "exploded", "-777592863")
                .pathParam( "nonEmpty", "-787864396,968195964,-941125629")
                .pathParam( "empty", "-479634177")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", ",-290067183,-159033827,-879373065,-998839893,1007387311,840979781,718184765,-719901843,212473142,-466899496,608407672,-599674523")
                .pathParam( "exploded", ",-660351823,-362664233")
                .pathParam( "nonEmpty", ",226034055,-884703973,226034055")
                .pathParam( "empty", ",-97868370,-279296209,485709141,-216666114,-453603517,-935803584,963242121")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "0")
                .pathParam( "exploded", "0")
                .pathParam( "nonEmpty", "0,-790520378,464352219")
                .pathParam( "empty", "0")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "247662958,206516015,206516015")
                .pathParam( "exploded", "339122953,339122953,-164249220")
                .pathParam( "nonEmpty", "597556810,-739813784,-10240067,47029742,757335484,822581821,-956279549,220628827,-858529578,895300504,-739813784,722180891,170367393,683319538,703520610")
                .pathParam( "empty", "117826646,117826646")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-1044207262")
                .pathParam( "exploded", "-210497736")
                .pathParam( "nonEmpty", "-779353904,665084706,396090616")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-581166028")
                .pathParam( "exploded", "-613571985")
                .pathParam( "nonEmpty", "-551185881,168692740,-987473178")
                .pathParam( "empty", "true")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-777374064")
                .pathParam( "exploded", "-365727516")
                .pathParam( "nonEmpty", "-527133846,772732844,-328103540")
                .pathParam( "empty", "-213.4")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-213474350")
                .pathParam( "exploded", "-480690895")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", "-449024820")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-194716814")
                .pathParam( "exploded", "-978628238")
                .pathParam( "nonEmpty", "-545.9")
                .pathParam( "empty", "-448965344")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-513574421")
                .pathParam( "exploded", "-272279337")
                .pathParam( "nonEmpty", "-752797874,-752797874")
                .pathParam( "empty", "-621459142")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-728126616")
                .pathParam( "exploded", "-321574503")
                .pathParam( "nonEmpty", "hnd,909.0,edrwghygiso,764.4,485495228,998536309")
                .pathParam( "empty", "-791141610")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "")
                .pathParam( "exploded", "-41580670")
                .pathParam( "nonEmpty", "-732336218,109949038,-1044123185")
                .pathParam( "empty", "-1027669540")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "395.2")
                .pathParam( "exploded", "-57818659")
                .pathParam( "nonEmpty", "-141106705,1061086333,-945685656")
                .pathParam( "empty", "-796367007")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "ximwsaxjsd,5as,dlzzomzfuknzqlju,315.3,gmbpaka,-970")
                .pathParam( "exploded", "-221863041")
                .pathParam( "nonEmpty", "-461472705,-748673657,551942108")
                .pathParam( "empty", "-887294233")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-867795673")
                .pathParam( "exploded", "")
                .pathParam( "nonEmpty", "-987923883,-267637067,-164428690")
                .pathParam( "empty", "-823710164")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-882107883")
                .pathParam( "exploded", "(#ur5")
                .pathParam( "nonEmpty", "-223882716,894636233,-781422867")
                .pathParam( "empty", "-277231493")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "-423575356")
                .pathParam( "exploded", "T.n+,e/QX")
                .pathParam( "nonEmpty", "-426301194,655510060,-188352479")
                .pathParam( "empty", "-365942331")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0,height,0,cmpwhef,-200")
                .pathParam( "exploded", "width=0,height=0,qrvgh=true")
                .pathParam( "nonEmpty", "width,0,height,0,imukwskokuxrpx,true,dexxjvohnhiu,Y>.Sk")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,")
                .pathParam( "exploded", "width=")
                .pathParam( "nonEmpty", "height,")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "height,,idy,")
                .pathParam( "exploded", "height=,adqbzdj=")
                .pathParam( "nonEmpty", "width,,kdsyxirbchglysq,:\\#EOdZ,q,slwsgotyzi,true,paqfmjf,0'")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,724595442")
                .pathParam( "exploded", "width=10699369")
                .pathParam( "nonEmpty", "width,984602434,height,1022042340")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "height,855122468,oxaahpnkiy,-191.1,wtokegnlt,9@!}Gd,ljqlwttimhkxjjh,true")
                .pathParam( "exploded", "height=697375612,fmpapzhxplitxbnf=3dRq9;7,wr=qI^e,4Hd@2,2,cf=w RJ=\"8")
                .pathParam( "nonEmpty", "qnwnhrx,true,ioruzhsoollf,{C,2,OK")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "786.1")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,YO&=,T>{hfh,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,-1,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,708.2")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,-1")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "true")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,igZ6Gm")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,-1")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0,height,")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0,height,-1")
                .pathParam( "exploded", "width=0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "-440.4")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0,height=")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "width,0")
                .pathParam( "exploded", "width=0,height=-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "A")
                .pathParam( "nonEmpty", "S0)")
                .pathParam( "empty", "$")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "hhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=")
                .pathParam( "nonEmpty", "\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN")
                .pathParam( "empty", "IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "}")
                .pathParam( "nonEmpty", "o%z")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "L")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ".")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "P")
                .pathParam( "nonEmpty", "f%")
                .pathParam( "empty", "z")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "K{2")
                .pathParam( "empty", "h")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
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

    private static Map<String,String> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( toMap( Header::getName, Header::getValue));
    }
}
