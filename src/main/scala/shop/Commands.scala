package shop
import database.Table
import database.queryT
import database.Field
import database.Not
import database.PP_SQL_Table_Filter
import database.PP_SQL_Table_Update
import database.*

class Commands(productsTable: Table) {
    val shoppingCart = Table("shoppingCart", "name,quantity,price")

    // 3.1.1
    def START_SHOPPING() : Table = shoppingCart
    // 3.1.2
    def ADD_PRODUCT(shopList: Table, barcode: String, quantity: Int): Table = {
        val product = productsTable.data.find(r => r("Barcode") == barcode).get

        shopList.insert(Map("name"->product("Name"), "quantity"->quantity.toString, "price"-> product("Price")))
    }
    // 3.1.3
    def DELETE_PRODUCT(t: Table, name: String): Table = {
        DeleteRow(t, t.data.find(r => r("name") == name).get).eval.get
    }
    // 3.1.4
    def EDIT_QUANTITY(t: Table, name: String, newQuantity: Int): Table = {
        UpdateRow(t, Field("name", _ == name), Map("quantity"->newQuantity.toString)).eval.get
    }
}
