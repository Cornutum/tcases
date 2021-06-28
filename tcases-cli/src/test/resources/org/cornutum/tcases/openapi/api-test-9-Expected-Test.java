package org.cornutum.readonly;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ReadOnlyTest extends MyBaseClass {

    @Test
    public void postObject_Param0Defined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "96TliL!Z,f0p3C3H/C~ Y; Cq-g>!iw?gA0g/rFP3jb")
            .queryParam( "delta", "V|rF[@`]UGRf")
            .queryParam( "iyuhhng", "k")
            .queryParam( "qmjso", "")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0Defined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "")
            .queryParam( "dh", "E\"")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "")
            .queryParam( "delta", "&buS(c6qP~elSQr.el.yIbq<f:oyE=1N *()q8 ;gf=}K.Rx*=2:,N*aKO\"")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "QT(zwNL'PRBj]X+!<k<r?4IBML~>,GI!wPYs*p.Q85z_-M\"krlKsIbRvN2 ]-")
            .queryParam( "jkqpuclwpkn", "98")
            .queryParam( "wuthsrqwmnmhbooc", "ijxxing,")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0Type_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "param0", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0Type_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "param0", "true")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertyCount_Is_Lt_2() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Property-Count=< 2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesAlphaDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "alpha", "RmV^O)gcx]wBcJ2MA91&Z8xtl1F-l<E{-~NtTaa({6{Ajw9hcHdtE{8[Vk'2C>_dd zR'9vB{A6Og")
            .queryParam( "delta", "")
            .queryParam( "ztwpanoxgsqkjve", "fltocdnc,-567,mj,z,tddvadexwwd,630.2")
            .queryParam( "hxnzabm", "true")
            .queryParam( "wqbwor", "true")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.alpha.Defined=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "")
            .queryParam( "delta", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.bravo.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesCharlieDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "")
            .queryParam( "charlie", "|ShV(C]BTOCBUi]T\">0KaKkBt|Ba/")
            .queryParam( "rqfuptx", "rmx,rsz")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.charlie.Defined=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "")
            .queryParam( "tejmdfv", "zjknigcd,-795,rnixecummrmos,tD<G<dx,ofhgtynxqndjxf,329.3")
            .queryParam( "a", "-275.2")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.delta.Type=null
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private String forTestServer() {
        return forTestServer( null);
    }

    private String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
