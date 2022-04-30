package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import io.restassured.builder.MultiPartSpecBuilder;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiEncodingsTest {

    @Test
    public void postMultipart_contentTypes_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-4015880461615352075.2")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=-3764606968942181708.9&1=-3948954589302173570.8")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          43,  48,  86, 121, 121, 117,  53,  68, 119,  88, 119,  99, 116,  72,  65, 104,
                          81, 109,  48,  53,  79,  65,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "221561836237695255.5")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=C%3BI&tags=o%27%3D&tags=ago")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          75,  69, 112,  71, 102,  48, 115,  49,  52,  90,  69,  77,  90, 107, 104,  48,
                         104,  69,  66,  48, 110,  65,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=612074252259956970.7&1=-4026702765476585609.3")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-4285336973875279358.0")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "city=%26t**%3AyTTsR0m%5EsX%21")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         108,  73,  74, 108,  47,  89, 105,  73,  90, 112,  66, 117, 103, 116, 113, 109,
                          74,  86,  86,  52,  74,  65,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=974148905&city=1P*T7R64%3CUJN&tags=")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=-4337740899909175924.2&1=-4337740899909175924.2")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "tags=vr%29")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         101,  80,  98, 112, 109,  74,  69,  81,  97,  49,  88, 108,  72, 121,  51, 101,
                          57, 121,  82, 107, 115, 103,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/xml")
            .request().body( "-867")
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "null")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3786396932369684979.0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "true")
                .mimeType( "application/json")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4301032283436116768.0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "integer=769")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3966565507799172640.9&2=-3966565507799172640.9")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0&1=-4513142926853007872.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=rx%2C&1=-4453020789191268768.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4286253177019457924.5")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "string=")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3839538486714012809.4")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3961000203219933904.7")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=%5E%26y%22%3D9%21&city=%7EiR%3DAxV&city=g%26V%3D.m%7CQ")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3926680665167424605.9")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=d%5BVP3%28%7Dhf%3E%26%7BK%2B%25%28%60")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3928314110429319410.5")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code&city=")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4096970368387288199.0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=true&city=")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4132798941531626425.3")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=0&city=")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4167035433357921910.3")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4171431384351149506.4")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=true")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3833874904839485019.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=J9%5B&tags=f%2F%29&tags=f%2F%29&tags=Vof")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4561570444051769799.8")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags&tags=%40%3BT&tags=%25Nm")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-3995331446264199147.8")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=308.7&tags=FlO&tags=3%2FN")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4151079189182544799.8")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=%28%26&tags=Y%3F%5C&tags=enf")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4512034527367741225.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&tags=%5Bw%3FX&tags=S4v&tags=%5Blr")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4498119522555696773.0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "code=1&city=&ecn=340&eqcxmtafjqg=&zdeypmccmg=-784")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4287669161698270000.4")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4005916464835083321.8")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4443645344208762164.7")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "-474")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4186567294095977373.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          69,  69,  54,  78,  73,  87, 107,  67,  51, 100,  72,  85, 100,  99,  80,  82,
                          86, 121, 101,  73
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4226229991835249842.9")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          79, 122, 104,  70,  73,  85,  83,  69,  88,  55,  79,  79, 102,  88,  73,  71,
                          71, 110, 114, 118,  53, 106, 119,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0=0&1=-4559539283013593225.2")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[\"T\",\"\",\"E\"]")
                .mimeType( "text/plain")
                .controlName( "s")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"ohysnoz\":-551,\"jpwzxtc\":\"u\"}")
                .mimeType( "application/json")
                .controlName( "ymedkincneaqkzh")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_contentTypes")
        .then()
            // Body.multipart-form-data.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-3918308422830004354.9")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[-3850322090996352648.3,-4398369311166024635.0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          74, 118,  84,  89,  50, 121, 111, 107, 100,  90,  74,  52,  86,  71, 108,  98,
                          54, 118,  85,  55,  76,  65,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "416082164823058236.7")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"N=q\",\"rKs\",\"N;F\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          69, 107,  52,  81, 115, 109,  68,  52,  55,  86, 113,  48, 118,  97,  85,  65,
                         112,  88,  75,  79,  85,  81,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[718740597518618862.5,-4016888682510821057.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-4427506999282114717.0")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"fDraOI:B' \\\\Y@]$R\"}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          88, 101, 104,  51, 106, 116,  85, 114,  75,  56,  82,  98,  52,  97, 118,  77,
                          72, 112, 100,  51, 116, 119,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1027163580,\"city\":\"C]{ccx&h8'%\",\"tags\":[]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[-3811335743119393234.8,-3811335743119393234.8]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "0")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"tags\":[\" :f\",\" :f\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         110,  86, 118, 120, 119,  49,  73, 105, 110, 110,  57, 121,  54,  51, 116,  74,
                          75,  85,  43,  66,  73, 119,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_default_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "text/plain")
            .request().body( "true")
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4400533109304594458.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[]")
                .mimeType( "text/plain")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4292182829844541845.2]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "112")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4377191655555601772.1,-4377191655555601772.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[null,-4607386633012760609.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[true,-4112378271795984939.8]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-3769957374373657464.2]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "917.5")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4483092028650488105.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":null}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4186042882329482874.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":-407}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4509542086416850416.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"1`u/M&\\\\9L1#X.;bRv\"}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4590146899078678513.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":null,\"city\":\"\"}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4018089148087288193.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":\"D9Y:#i(\",\"city\":\"\"}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4194093586596252606.3]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":0,\"city\":\"\"}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4154198394222776724.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":null}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-3713511221973133157.5]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":{\"zovfsrdezy\":\"tQ65\"}}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-3870732540043791567.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"V'<\",\"Pk@\",\"AX/\",\"Pk@\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4473357202223679782.5]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[null,\"9ZI\",\":sj\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4136729250249934624.3]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[437.5,\"PWM\",\"*b9\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4059840097555610074.9]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"s\\\"\",\"ZtD\",\"4i!\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4136956984180014442.0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"u(>u\",\"lVu\",\"&\\\"O\"]}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4177489741014890564.5]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"gisfvhtrjouvwyen\":601,\"ntvwcyaf\":true}")
                .mimeType( "application/json")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,-3820132090201626135.7]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4505905942159561537.2]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4162757515595773646.3]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "true")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4139008522903747137.8]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         100,  71,  81,  57,  89,  74, 120,  74,  75,  55, 116, 117, 120,  90, 109,  66,
                          88, 113, 119,  86
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4241940734128988095.9]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          80,  86,  75,  82, 111,  97,  98,  49, 122, 105,  81,  55, 122,  87, 115,  71,
                          86, 110,  90,  53, 114,  71, 103,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-3759959197270384086.9]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "-63")
                .mimeType( "text/plain")
                .controlName( "ruigrpp")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[]")
                .mimeType( "text/plain")
                .controlName( "qnbbqeatzghyumw")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_default")
        .then()
            // Body.multipart-form-data.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=-4012830596596335819.4")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[-3962348462476220734.6,-4584166404293578753.4]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         103,  47, 120,  50, 111, 111,  72,  52,  55,  67, 108, 116, 113, 116,  79,  77,
                          43,  97, 118, 109,  54, 119,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=30963843544175729.6")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"93a\",\"..+\",\"~/W\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         119,  70,  87,  99,  76, 114,  50, 104, 103,  49,  54,  57,  90,  97, 101, 122,
                         107,  99,  78, 113,  56, 103,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[701205971495925088.3,-3889826677265002147.6]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsSize_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=-4115721852761566776.1")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"[`-jb> !=HQFRG=3\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[-4294546549014250054.4,-4294546549014250054.4]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          70,  69,  70,  73,  67,  55,  73,  53,  88, 116, 118, 121, 107,  86, 104,  85,
                         101,  69, 107, 107, 113, 119,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4371099679899553113.7]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=0")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":531801075,\"tags\":[]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[830275648505669561.1,830275648505669561.1]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         115, 110, 117, 121,  66, 117,  68, 110, 105,  82,  68, 118,  55,  43,  52,  83,
                          73,  55, 120,  67,  83,  81,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"<mWdA$[T\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "number=607902570994801888.3")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":1,\"tags\":[\"?=f\",\"?=f\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "[-4527808543417837209.5,-3701014693755876925.9]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         110, 114, 103,  99, 121,  55,  48,  83,  51, 117,  81,  79, 117,  57,  54,  52,
                          73,  88, 100,  48,  90,  81,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postMultipart_headers_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/json")
            .request().body( "true")
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "boolean=true")
                .mimeType( "application/x-www-form-urlencoded")
                .controlName( "elevation")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-561")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,-4315130485127470611.5,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[null,null]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[{},{}]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "[0,0]")
                .mimeType( "text/plain")
                .controlName( "lat/long")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "'ImbvAJ")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":null}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":-942.7}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"-+WnXb6>Wng\\\\z|uJQ\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":null,\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":{},\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"code\":0,\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":null}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":{\"zsijwdtwlckbsr\":-773}}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"g^v\",\"z i\",\"g^v\",\"Zmf\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[null,\"g;N\",\"h2h\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[49.6,\"E5J\",\"d_0\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"|/\",\"t5m\",\"q`t\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"uQ+\\\\\",\"rl8\",\"iH`\"]}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\",\"drgwxxvtqfbnqsx\":441,\"qt\":\"&CJY9lCI\",\"qckohbocx\":984}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueMatchesPattern_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Matches-Pattern=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueMatchesPattern_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "{\"city\":\"\"}")
                .mimeType( "text/plain")
                .controlName( "address")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Matches-Pattern=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder( "-267")
                .mimeType( "text/plain")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                         121,  69,  78,  67,  79, 109,  53,  47,  66,  67,  69,  43, 105,  83, 113, 116,
                          68,  77,  79,  48
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          89,  66, 112,  76, 114, 111,  97,  90, 121,  75, 120,  79,  70, 104,  43, 116,
                          90,  99,  70,  99, 116, 119,  77,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "multipart/form-data")
            .multiPart(
                new MultiPartSpecBuilder(
                    new byte[] {
                          56, 101, 101, 114,  99, 109, 117,  76, 103,  99,  43,  50, 112, 104,  98,  81,
                          43, 111,  73, 113, 121,  81,  61,  61
                    })
                .mimeType( "application/octet-stream")
                .controlName( "map-digest")
                .emptyFileName()
                .build())
            .multiPart(
                new MultiPartSpecBuilder( "343.5")
                .mimeType( "text/plain")
                .controlName( "nryhbpwocyvwet")
                .emptyFileName()
                .build())
        .when()
            .request( "POST", "/multipart_headers")
        .then()
            // Body.multipart-form-data.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-3998686662832137669.0")
            .formParam( "lat/long", "-3831399503999759339.7")
            .formParam( "lat/long", "-3896946869424179604.7")
            .formParam( "map-digest", "/k5l0sSjM+6PV0ybEXVtaA==")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "13861742107148630.7")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", "/Fz,.bS,!Al")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "0")
            .formParam( "map-digest", "LqamvNvZl/z68ZKFrwbLCQ==")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "417098339751014037.5")
            .formParam( "lat/long", "-4610839891197091495.2")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-4596298842289608675.7")
            .formParam( "city", "3:gY'aO2@Kf=v6k\"")
            .formParam( "map-digest", "7cX6sbGyto3edm1Oo642kQ==")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "89167223")
            .formParam( "city", "z6V\\34Y")
            .formParam( "tags", "")
            .formParam( "lat/long", "-4458102162249783838.8")
            .formParam( "lat/long", "-4458102162249783838.8")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
            .formParam( "tags", "=pb")
            .formParam( "map-digest", "1i+jwPlF6+I2VrYcYa1gyQ==")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "text/xml")
            .request().body( "{\"ointzrrcflk\":[\")EI]\",\"q:A4ME@N\",\"r^@-w)0\"],\"zqespwtlklylpq\":[]}")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "-56.1")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3742893752063002545.3")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "true")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4473425286276371988.2")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", (String) null)
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "A+Eg%{")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4409995165872643341.1")
            .formParam( "lat/long", "-4409995165872643341.1")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", (String) null)
            .formParam( "lat/long", "-3901628421304067287.0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "lzg)71Kd")
            .formParam( "lat/long", "-3877446308665050644.6")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4334444961244313119.4")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "true")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4042043636966579916.8")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4143019679704202612.4")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "89.4")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3865946472481555785.1")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "-<jvlD#VY~7f)`-Q\"")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4232760054990494991.8")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", (String) null)
            .formParam( "city", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4129766631341243123.5")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "true")
            .formParam( "city", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4438664881108900341.7")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "0")
            .formParam( "city", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4175951182263845715.1")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4027586750120666447.5")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", "-893.0")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4225908759673784845.9")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", ":u.,/Fs,d i,d i")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4500385512417742411.9")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", ",X~s,1Vm")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4464217779618864188.7")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", "-57,G-U,y I")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3850403813816698179.0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", "Uz,Y8J,O+i")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4352639283157696290.2")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "tags", "5=7&,lUU,'1V")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4076716910919205839.8")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "code", "1")
            .formParam( "city", "")
            .formParam( "fzqz", "^0f^")
            .formParam( "hqhnsvgtwdmuh", "true")
            .formParam( "rlyh", "kiemzxwdkrd,-641.2,rgmgykqn,")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4099017338619395479.0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4494640213686952260.1")
            .formParam( "map-digest", (String) null)
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3911348252849535140.9")
            .formParam( "map-digest", "$z@")
            .formParam( "map-digest", "MJH9H")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4608041651028345882.6")
            .formParam( "map-digest", "it+O2QRqxQkUfSo9oNG0")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3899969156489621128.1")
            .formParam( "map-digest", "gWDzedxZBWgwklJ7PAnx7rA=")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3713639263327634238.7")
            .formParam( "p", "j\\.a-wa\\")
        .when()
            .request( "POST", "/urlencoded_default")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-3979085128911043317.3")
            .formParam( "lat/long", "-3856619558996624188.4")
            .formParam( "lat/long", "-4276789922876724548.4")
            .formParam( "map-digest", "KmODdXTZ+0jUZcxjZyPrUQ==")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "911360481490011019.4")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "-SR,p{v,f}=")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "0")
            .formParam( "map-digest", "RAKlaApmVRFAxnk1GauGtg==")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "556046036484608815.0")
            .formParam( "lat/long", "-3765676371040112417.2")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-4071337164215218772.8")
            .formParam( "address[city]", ",;<`]$x}{Mo, h<%")
            .formParam( "map-digest", "LQ6TktLWQuh8n3SNDWGXqQ==")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "549261324")
            .formParam( "address[city]", ",")
            .formParam( "address[tags]", "")
            .formParam( "lat/long", "-4247554277882765848.7")
            .formParam( "lat/long", "-4247554277882765848.7")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
            .formParam( "address[tags]", "bzz,bzz")
            .formParam( "map-digest", "K30Z9Peq5Llk0xwRLN5rrQ==")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "text/xml")
            .request().body( "{\"s\":true,\"wzauwxabwfy\":-676.2}")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", "iR")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3950022034912146813.3")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "<+&&SN")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3895657449868675534.6")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", (String) null)
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "983.5")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4471412725602966539.8")
            .formParam( "lat/long", "0")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", (String) null)
            .formParam( "lat/long", "-4184035724417160761.8")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "true")
            .formParam( "lat/long", "-4191853462408770088.8")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4507305500796112203.3")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "true")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3810767836046834382.6")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4180219414110481848.2")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "p,true,z,?m2")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3831258360150208307.4")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "hT=y$_hS[C*8UP.rh")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4329225184491036220.5")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", (String) null)
            .formParam( "address[city]", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3821862262599397631.8")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "nxwn,115,eifqkddegzt,true")
            .formParam( "address[city]", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3749175381718860829.6")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "0")
            .formParam( "address[city]", "")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4414907521189103862.5")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", (String) null)
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3715324994778925667.4")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "yvsvfksciwj,-875,bjevm,-559")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3792737117472895549.6")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "~QH,~QH,d^k,&,%")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4241363778820548281.9")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", ",$U=,yBy")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3868207668815503276.2")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "true,A+?,Zau")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4312922773138190410.1")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "]U,< >,23T")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4280531712573783964.8")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[tags]", "lAR#,*J.,hO0")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4275724407026027571.2")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address[code]", "1")
            .formParam( "address[city]", "")
            .formParam( "address[ehmonwvv]", "~s@;g!h")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4275636487329301141.7")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-3777016484881642222.0")
            .formParam( "map-digest", (String) null)
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4540438693121728488.9")
            .formParam( "map-digest", "G3{Qx#&")
            .formParam( "map-digest", "m")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4554147447997542600.5")
            .formParam( "map-digest", "FMfBjv9ZNiBydC2f2BZZ")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4003619112190346977.3")
            .formParam( "map-digest", "PuZFxYsOlGSBhYeqPt4WF/g=")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
            .formParam( "lat/long", "-4421242956268405566.3")
            .formParam( "iwefvtwyrzlqvr", "true")
        .when()
            .request( "POST", "/urlencoded_exploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-3697477807719600901.4")
            .formParam( "address", "")
            .formParam( "lat/long", "-3941518147795005315.9 -4472747738610876664.2")
            .formParam( "map-digest", "Cmb7pScnzF0c/KqOb+U59Q==")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "690659103122832905.4")
            .formParam( "address", "code,1,city,,tags,sx/,~ =,.^m")
            .formParam( "lat/long", "0 0")
            .formParam( "map-digest", "aCHtrV8Fqwb4nr1Ryw8kxQ==")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "36973612924832808.3 -4244529446206111807.5")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "-4270172250060225302.9")
            .formParam( "address", "city,TbkN:L0f#k -Es0E")
            .formParam( "map-digest", "E/1aSuRvOxjNJ8Wxo+4UAQ==")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,496452278,city,5TC>Ae1iXoX,tags,")
            .formParam( "lat/long", "-4009914991350942732.2 -4009914991350942732.2")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "0")
            .formParam( "address", "tags,%s\"")
            .formParam( "map-digest", "YUSbAQpZGZWsfsi3QCZY9Q==")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .contentType( "text/plain")
            .request().body( "{\"hbnnwqyeuciyk\":654,\"lx\":-167.0,\"zrvimr\":611}")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "boolean", "true")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", (String) null)
            .formParam( "lat/long", "0 -4257926504886947356.3")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "elevation", "true")
            .formParam( "lat/long", "0 -3996180626982454036.0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", (String) null)
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "-940.8")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -3767332928424115496.7 0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", " -4386099657086262915.1")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", " -3703340927199636833.8")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", (String) null)
            .formParam( "lat/long", "0 -3778053685970423944.4")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "y")
            .formParam( "lat/long", "0 -3977605567165807846.8")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,")
            .formParam( "lat/long", "0 -3989373869659181466.7")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,-948")
            .formParam( "lat/long", "0 -4057248465908399274.6")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,r$2z\"'if2Tq^1k0xN")
            .formParam( "lat/long", "0 -4546782878233797320.1")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,,city,")
            .formParam( "lat/long", "0 -3922872433273232576.8")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,psywsmcve,true,haziqgogy,5~nv{fxS,krihnhspc,,city,")
            .formParam( "lat/long", "0 -3722351607058499060.4")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,0,city,")
            .formParam( "lat/long", "0 -3899905553795101571.0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,")
            .formParam( "lat/long", "0 -3853346367494165704.6")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,")
            .formParam( "lat/long", "0 -4336524928156913218.8")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,(hl,p~],\\n>,\\n>")
            .formParam( "lat/long", "0 -4395470632066473820.3")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,,JQU,oA7")
            .formParam( "lat/long", "0 -4206349109306439332.6")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,true,7ik,j.<")
            .formParam( "lat/long", "0 -4382788430634783907.0")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,8Q,teN,AjR")
            .formParam( "lat/long", "0 -3702445014294156660.4")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,tags,.zU',![H,uB<")
            .formParam( "lat/long", "0 -4218496435396096581.3")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "address", "code,1,city,,so,874")
            .formParam( "lat/long", "0 -4444179334043026213.3")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -3713414574598782004.9")
            .formParam( "map-digest", (String) null)
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -4435924864779329982.8")
            .formParam( "map-digest", "-756.7")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -3798147547566549853.1")
            .formParam( "map-digest", "+k/icsBOVwTaHuNHgwKe")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -4564923897878342232.6")
            .formParam( "map-digest", "tmJ+oL+C2MJQok0dmGbj96s=")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "lat/long", "0 -3863311061174700019.4")
            .formParam( "wlsa", "true")
        .when()
            .request( "POST", "/urlencoded_unexploded")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
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
