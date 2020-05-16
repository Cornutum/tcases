package org.cornutum.readonly;


import org.junit.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ReadOnlyTest extends MyBaseClass {

    @Test
    public void postObject_Param0Defined_Is_Yes() {
        given()
            .queryParam( "bravo", "96TliL!Z,f0p3C3H/C~ Y; Cq-g>!iw?gA0g/rFP3jb")
            .queryParam( "delta", "V|rF[@`]UGRf")
            .queryParam( "iyuhhng", "k")
            .queryParam( "qmjso", "")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postObject_Param0Defined_Is_No() {
        given()
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoDefined_Is_No() {
        given()
            .queryParam( "delta", "")
            .queryParam( "dh", "E\"")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoValueLength_Is_0() {
        given()
            .queryParam( "bravo", "")
            .queryParam( "delta", "&buS(c6qP~elSQr.el.yIbq<f:oyE=1N *()q8 ;gf=}K.Rx*=2:,N*aKO\"")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaDefined_Is_No() {
        given()
            .queryParam( "bravo", "QT(zwNL'PRBj]X+!<k<r?4IBML~>,GI!wPYs*p.Q85z_-M\"krlKsIbRvN2 ]-")
            .queryParam( "jkqpuclwpkn", "98")
            .queryParam( "wuthsrqwmnmhbooc", "ijxxing,")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postObject_Param0Type_Is_Null() {
        given()
            .queryParam( "param0", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0Type_Is_NotObject() {
        given()
            .queryParam( "param0", "true")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertyCount_Is_Lt_2() {
        given()
            .queryParam( "delta", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Property-Count=< 2
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesAlphaDefined_Is_Yes() {
        given()
            .queryParam( "alpha", "RmV^O)gcx]wBcJ2MA91&Z8xtl1F-l<E{-~NtTaa({6{Ajw9hcHdtE{8[Vk'2C>_dd zR'9vB{A6Og")
            .queryParam( "delta", "")
            .queryParam( "ztwpanoxgsqkjve", "fltocdnc,-567,mj,z,tddvadexwwd,630.2")
            .queryParam( "hxnzabm", "true")
            .queryParam( "wqbwor", "true")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.alpha.Defined=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoType_Is_Null() {
        given()
            .queryParam( "bravo", "")
            .queryParam( "delta", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.bravo.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoType_Is_NotString() {
        given()
            .queryParam( "bravo", "")
            .queryParam( "delta", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.bravo.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesCharlieDefined_Is_Yes() {
        given()
            .queryParam( "delta", "")
            .queryParam( "charlie", "|ShV(C]BTOCBUi]T\">0KaKkBt|Ba/")
            .queryParam( "rqfuptx", "rmx,rsz")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.charlie.Defined=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaType_Is_Null() {
        given()
            .queryParam( "delta", "")
            .queryParam( "tejmdfv", "zjknigcd,-795,rnixecummrmos,tD<G<dx,ofhgtynxqndjxf,329.3")
            .queryParam( "a", "-275.2")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.delta.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaType_Is_NotString() {
        given()
            .queryParam( "delta", "true")
            .queryParam( "ljips", "true")
            .queryParam( "qvpzrlhxfi", "lbmnqax,-707.1")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.delta.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
