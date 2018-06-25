package semaforo;

public class ProdutorConsumidor {

	public static void main(String[] args) {
		Drop drop = new Drop();
		Produtor produtor = new Produtor(drop);
		Consumidor consumidor = new Consumidor(drop);
		Thread p = new Thread(produtor);
		Thread c = new Thread(consumidor);
		p.start();
		c.start();
	}

}
